import { Test, TestingModule } from '@nestjs/testing';
import { AuthenticationModule } from 'src/authentication/authentication.module';
import { PrismaModule } from 'src/prisma/prisma.module';
import { PrismaService } from 'src/prisma/prisma.service';
import { UserModule } from 'src/user/user.module';
import SetupApp from 'test/setup-app';
import TestPrismaService from 'test/test-prisma.service';
import { HttpStatus, INestApplication } from '@nestjs/common';
import { ListingImage, User } from '@prisma/client';
import { UserService } from 'src/user/user.service';
import * as request from 'supertest';
import {
	CreateToyListing,
	ToyListing,
	UpdateToyListing,
} from 'src/prisma/models';
import { ImagesModule } from 'src/image/image.module';
import { ToyListingModule } from 'src/toy-listing/toy-listing.module';
import { existsSync, readFileSync, rm, rmSync } from 'fs';
import { ToyListingResponse } from 'src/toy-listing/toy-listing.response';
import { ImageService } from './image.service';
import { Readable } from 'stream';

describe('Image Controller', () => {
	let user1: User;
	let user1Listing: ToyListing;
	let user2: User;
	let user1Token: string;
	let user2Token: string;
	let app: INestApplication;
	let module: TestingModule;
	let userService: UserService;
	let image: ListingImage;

	beforeAll(async () => {
		module = await Test.createTestingModule({
			imports: [
				AuthenticationModule,
				UserModule,
				ImagesModule,
				PrismaModule,
				ToyListingModule,
			],
			providers: [PrismaService],
		})
			.overrideProvider(PrismaService)
			.useClass(TestPrismaService)
			.compile();
		app = await SetupApp(module);
		await module.get(PrismaService).onModuleInit();
		userService = module.get(UserService);
		user1 = await userService.createUser({
			email: 'first@mail.com',
			firstName: 'First',
			lastName: 'Last',
			userName: 'firstName',
			password: 'MyPassword',
			phone: '0123456789',
		});
		user2 = await userService.createUser({
			email: 'second@mail.com',
			firstName: 'Second',
			lastName: 'Last',
			userName: 'secondLast',
			password: 'MyPassword2',
			phone: '0223456789',
		});
		user1Token = (
			await request(app.getHttpServer()).post(`/auth/login`).send({
				username: 'firstName',
				password: 'MyPassword',
			})
		).body.accessToken;
		user2Token = (
			await request(app.getHttpServer()).post(`/auth/login`).send({
				username: 'secondLast',
				password: 'MyPassword2',
			})
		).body.accessToken;
		user1Listing = (
			await request(app.getHttpServer())
				.post(`/listings`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.send({
					condition: 'Good',
					title: 'Silent Hill 2 - PS2 Game with Box and Booklet',
					description: 'Working Game. Some light scratches.',
					type: 'VideoGame',
					postCode: 75000,
				} satisfies CreateToyListing)
		).body;
		rm('./data/listings/', { recursive: true }, () => {});
	});

	describe('Create An Image', () => {
		it('Should Fail, as the user is not authentified', async () => {
			return request(app.getHttpServer())
				.post(`/images/${user1Listing.id}`)
				.attach('file', 'test/assets/image.png')
				.expect(HttpStatus.UNAUTHORIZED);
		});
		it('Should Fail, as the user is not the owner of the listing', async () => {
			return request(app.getHttpServer())
				.post(`/images/${user1Listing.id}`)
				.set({ Authorization: `Bearer ${user2Token}` })
				.attach('file', 'test/assets/image.png')
				.expect(HttpStatus.UNAUTHORIZED)
				.expect((res) => {
					expect(res.body.message).toBe(
						'You can not create an image for a listing that is not yours.',
					);
				});
		});
		it('Should Fail, as the listing does not exist', async () => {
			return request(app.getHttpServer())
				.post(`/images/-1`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.attach('file', 'test/assets/image.png')
				.expect(HttpStatus.NOT_FOUND);
		});
		it('Should Fail, as the binary payload is invalid', async () => {
			return request(app.getHttpServer())
				.post(`/images/${user1Listing.id}`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.attach('file', 'test/setup-app.ts')
				.expect(HttpStatus.UNPROCESSABLE_ENTITY);
		});
		it('Should Create an Image Row and Save the File', async () => {
			return request(app.getHttpServer())
				.post(`/images/${user1Listing.id}`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.attach('file', 'test/assets/image.png')
				.expect(HttpStatus.CREATED)
				.expect((res) => {
					image = res.body;

					expect(image.id).toBeDefined();
					expect(image.blurhash.length).toBeGreaterThan(10);
					expect(image.listingId).toBe(user1Listing.id);
					expect(existsSync(`./data/listings/${image.id}.jpg`)).toBe(
						true,
					);
				});
		});
		it('Should Return The Image Info With The Listing ', async () => {
			return request(app.getHttpServer())
				.get(`/listings/${user1Listing.id}`)
				.expect(HttpStatus.OK)
				.expect((res) => {
					const response: ToyListingResponse = res.body;

					expect(response.images).toStrictEqual([image]);
				});
		});
	});
	describe('Stream An Image', () => {
		it('Should Fail, as the image does not exist', async () => {
			return request(app.getHttpServer())
				.get(`/images/-1`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.expect(HttpStatus.NOT_FOUND);
		});
		it('Should Fail, as the image file does not exist', async () => {
			let image2: ListingImage;
			await request(app.getHttpServer())
				.post(`/images/${user1Listing.id}`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.attach('file', 'test/assets/image.png')
				.expect(HttpStatus.CREATED)
				.expect((res) => {
					image2 = res.body;

					expect(existsSync(`./data/listings/${image2.id}.jpg`)).toBe(
						true,
					);
					rmSync(`./data/listings/${image2.id}.jpg`);
				});
			await request(app.getHttpServer())
				.get(`/images/${image2.id}`)
				.expect(HttpStatus.NOT_FOUND)
				.expect((res) => {
					expect(res.body.message).toBe('Image file not found');
				});
			await module.get(ImageService).deleteImage(image2.id);
		});
		it('Should Return The Image', async () => {
			return request(app.getHttpServer())
				.get(`/images/${image.id}`)
				.expect(HttpStatus.OK)
				.expect((res) => {
					expect(res.body).toEqual(
						readFileSync(`./data/listings/${image.id}.jpg`),
					);
				});
		});
	});
	describe('Delete An Image', () => {
		it('Should Fail, as the user is not authentified', async () => {
			return request(app.getHttpServer())
				.delete(`/images/${image.id}`)
				.expect(HttpStatus.UNAUTHORIZED);
		});
		it('Should Fail, as the user is not the owner of the listing', async () => {
			return request(app.getHttpServer())
				.delete(`/images/${image.id}`)
				.set({ Authorization: `Bearer ${user2Token}` })
				.expect(HttpStatus.UNAUTHORIZED)
				.expect((res) => {
					expect(res.body.message).toBe(
						'You can not delete an image that is not yours.',
					);
				});
		});
		it('Should Fail, as the image does not exist', async () => {
			return request(app.getHttpServer())
				.delete(`/images/-1`)
				.set({ Authorization: `Bearer ${user2Token}` })
				.expect(HttpStatus.NOT_FOUND);
		});
		it('Should Delete the Image Row and Delete the File', async () => {
			return request(app.getHttpServer())
				.delete(`/images/${image.id}`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.expect(HttpStatus.OK)
				.expect(() => {
					expect(existsSync(`./data/images/${image.id}.jpg`)).toBe(
						false,
					);
				});
		});
		it('Should Return The Image Info Without The Listing', async () => {
			return request(app.getHttpServer())
				.get(`/listings/${user1Listing.id}`)
				.expect(HttpStatus.OK)
				.expect((res) => {
					const response: ToyListingResponse = res.body;

					expect(response.images).toStrictEqual([]);
				});
		});
	});
});
