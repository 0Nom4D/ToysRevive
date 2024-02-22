import { Test, TestingModule } from '@nestjs/testing';
import { ToyListingModule } from './toy-listing.module';
import { AuthenticationModule } from 'src/authentication/authentication.module';
import { PrismaModule } from 'src/prisma/prisma.module';
import { PrismaService } from 'src/prisma/prisma.service';
import { UserModule } from 'src/user/user.module';
import SetupApp from 'test/setup-app';
import TestPrismaService from 'test/test-prisma.service';
import { HttpStatus, INestApplication } from '@nestjs/common';
import { User } from '@prisma/client';
import { UserService } from 'src/user/user.service';
import * as request from 'supertest';
import {
	CreateToyListing,
	ToyListing,
	UpdateToyListing,
} from 'src/prisma/models';
import { ImagesModule } from 'src/image/image.module';

const exceptedListingResponse = (response: ToyListing) => ({
	...response,
	images: [],
});

describe('ToyListing Controller', () => {
	let user1: User;
	let user1Listing: ToyListing;
	let user1Listing2: ToyListing;
	let user2: User;
	let user2Listing: ToyListing;
	let user1Token: string;
	let user2Token: string;
	let app: INestApplication;
	let module: TestingModule;
	let userService: UserService;

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
	});
	describe('Setup User Accounts', () => {
		it('Should Create 2 Accounts', async () => {
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
			expect(user1.firstName).toBe('First');
			expect(user2.firstName).toBe('Second');
		});
		it('Should Get Access tokens (User 1)', async () => {
			const loginResponse = await request(app.getHttpServer())
				.post(`/auth/login`)
				.send({
					username: 'firstName',
					password: 'MyPassword',
				});
			expect(loginResponse.statusCode).toBe(HttpStatus.CREATED);
			user1Token = loginResponse.body.accessToken;
			expect(user1Token).toBeDefined();
		});
		it('Should Get Access tokens (User 2)', async () => {
			const loginResponse = await request(app.getHttpServer())
				.post(`/auth/login`)
				.send({
					username: 'secondLast',
					password: 'MyPassword2',
				});
			expect(loginResponse.statusCode).toBe(HttpStatus.CREATED);
			user2Token = loginResponse.body.accessToken;
			expect(user2Token).toBeDefined();
		});
	});
	describe('Create Listing', () => {
		it('Should fail, as the request is not authentified', () => {
			return request(app.getHttpServer())
				.post(`/listings`)
				.send({
					condition: 'Good',
					title: 'Silent Hill 2 - PS2 Game with Box and Booklet',
					description: 'Working Game. Some light scratches.',
					type: 'VideoGame',
					postCode: 75000,
				} satisfies CreateToyListing)
				.expect(HttpStatus.UNAUTHORIZED)
				.expect((res) =>
					expect(res.body.message).toBe('Authentication Required'),
				);
		});
		it('Should Create A Listing for the first user', () => {
			return request(app.getHttpServer())
				.post(`/listings`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.send({
					condition: 'Good',
					title: 'Silent Hill 2 - PS2 Game with Box and Booklet',
					description: 'Working Game. Some light scratches.',
					type: 'VideoGame',
					postCode: 75000,
				} satisfies CreateToyListing)
				.expect(HttpStatus.CREATED)
				.expect((res) => {
					user1Listing = res.body;
					expect(user1Listing.id).toBeDefined();
					expect(user1Listing.title).toBe(
						'Silent Hill 2 - PS2 Game with Box and Booklet',
					);
					expect(user1Listing.ownerId).toBe(user1.id);
				});
		});
		it('Should Create A Second Listing for the first user', () => {
			return request(app.getHttpServer())
				.post(`/listings`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.send({
					condition: 'Good',
					title: 'Silent Hill 3 - PS3 Game with Box and Booklet',
					description: 'Working Game. Some light scratches.',
					type: 'VideoGame',
					postCode: 75000,
				} satisfies CreateToyListing)
				.expect(HttpStatus.CREATED)
				.expect((res) => {
					user1Listing2 = res.body;
					expect(user1Listing2.id).toBeDefined();
					expect(user1Listing2.title).toBe(
						'Silent Hill 3 - PS3 Game with Box and Booklet',
					);
					expect(user1Listing2.ownerId).toBe(user1.id);
				});
		});
		it('Should Create A Listing for the second user', () => {
			return request(app.getHttpServer())
				.post(`/listings`)
				.set({ Authorization: `Bearer ${user2Token}` })
				.send({
					condition: 'New',
					title: 'Nintendo Switch Pro Controller',
					description:
						'With Box and cable. 100% working. Battery still holds charge',
					type: 'Plastic',
					postCode: 22000,
				} satisfies CreateToyListing)
				.expect(HttpStatus.CREATED)
				.expect((res) => {
					user2Listing = res.body;
					expect(user2Listing.id).toBeDefined();
					expect(user2Listing.title).toBe(
						'Nintendo Switch Pro Controller',
					);
					expect(user2Listing.ownerId).toBe(user2.id);
				});
		});
	});
	describe('Like a Listing', () => {
		it('Should Not Like a Listing (not authentified)', () => {
			return request(app.getHttpServer())
				.post(`/listings/${user2Listing.id}/like`)
				.expect(HttpStatus.UNAUTHORIZED);
		});
		it('Should Not Like a Listing (is the owner)', () => {
			return request(app.getHttpServer())
				.post(`/listings/${user2Listing.id}/like`)
				.set({ Authorization: `Bearer ${user2Token}` })
				.expect(HttpStatus.BAD_REQUEST);
		});
		it('Should Like a Listing', () => {
			return request(app.getHttpServer())
				.post(`/listings/${user2Listing.id}/like`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.expect(HttpStatus.CREATED);
		});
		it('Should Dislike a Listing', () => {
			return request(app.getHttpServer())
				.post(`/listings/${user1Listing.id}/dislike`)
				.set({ Authorization: `Bearer ${user2Token}` })
				.expect(HttpStatus.CREATED);
		});
	});
	describe('Get One Listing', () => {
		it('Should Not Get A Listing (not authentified)', () => {
			return request(app.getHttpServer())
				.get(`/listings/${user1Listing.id}`)
				.expect(HttpStatus.UNAUTHORIZED);
		});
		it('Should Get A Listing (Authentified, not Owner)', () => {
			return request(app.getHttpServer())
				.get(`/listings/${user2Listing.id}`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.expect(HttpStatus.OK)
				.expect((res) => {
					expect(res.body).toStrictEqual(
						exceptedListingResponse(user2Listing),
					);
				});
		});
		it('Should fail, as the listing does not exist', () => {
			return request(app.getHttpServer())
				.get(`/listings/-1`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.expect(HttpStatus.NOT_FOUND)
				.expect((res) => {
					expect(res.body.message).toBe('Resource not found');
				});
		});
	});
	describe('Get Many Listings', () => {
		it('Should Get All listings', () => {
			return request(app.getHttpServer())
				.get(`/listings`)
				.expect(HttpStatus.OK)
				.set({ Authorization: `Bearer ${user1Token}` })
				.expect((res) => {
					const listings: ToyListing[] = res.body.items;

					expect(listings.length).toBe(3);
					expect(listings.at(0)).toStrictEqual(
						exceptedListingResponse(user2Listing),
					);
					expect(listings.at(1)).toStrictEqual(
						exceptedListingResponse(user1Listing2),
					);
					expect(listings.at(2)).toStrictEqual(
						exceptedListingResponse(user1Listing),
					);
				});
		});
		it('Should Get All listings, except the two firsts', () => {
			return request(app.getHttpServer())
				.get(`/listings?skip=2`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.expect(HttpStatus.OK)
				.expect((res) => {
					const listings: ToyListing[] = res.body.items;

					expect(listings.length).toBe(1);
					// By default, items should be sorted by date, descending.
					expect(listings[0]).toStrictEqual(
						exceptedListingResponse(user1Listing),
					);
				});
		});
		it('Should Sort Listings By Name (Desc)', () => {
			return request(app.getHttpServer())
				.get(`/listings?sortBy=title&oder=desc`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.expect(HttpStatus.OK)
				.expect((res) => {
					const listings: ToyListing[] = res.body.items;

					expect(listings.length).toBe(3);
					expect(listings.at(0)).toStrictEqual(
						exceptedListingResponse(user1Listing2),
					);
					expect(listings.at(1)).toStrictEqual(
						exceptedListingResponse(user1Listing),
					);
					expect(listings.at(2)).toStrictEqual(
						exceptedListingResponse(user2Listing),
					);
				});
		});
		it('Should Filter Listings By Condition', () => {
			return request(app.getHttpServer())
				.get(`/listings?condition=New`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.expect(HttpStatus.OK)
				.expect((res) => {
					const listings: ToyListing[] = res.body.items;

					expect(listings.length).toBe(1);
					expect(listings[0]).toStrictEqual(
						exceptedListingResponse(user2Listing),
					);
				});
		});
		it('Should Filter Listings By Type', () => {
			return request(app.getHttpServer())
				.get(`/listings?type=VideoGame`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.expect(HttpStatus.OK)
				.expect((res) => {
					const listings: ToyListing[] = res.body.items;

					expect(listings.length).toBe(2);
					expect(listings[0]).toStrictEqual(
						exceptedListingResponse(user1Listing2),
					);
					expect(listings[1]).toStrictEqual(
						exceptedListingResponse(user1Listing),
					);
				});
		});
		it('Should Get Listings By Owner', () => {
			return request(app.getHttpServer())
				.get(`/listings?ownerId=${user2.id}`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.expect(HttpStatus.OK)
				.expect((res) => {
					const listings: ToyListing[] = res.body.items;

					expect(listings.length).toBe(1);
					expect(listings[0]).toStrictEqual(
						exceptedListingResponse(user2Listing),
					);
				});
		});
		it('Should Get Liked Listings', () => {
			return request(app.getHttpServer())
				.get(`/listings?liked=true`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.expect(HttpStatus.OK)
				.expect((res) => {
					const listings: ToyListing[] = res.body.items;

					expect(listings.length).toBe(1);
					expect(listings[0]).toStrictEqual(
						exceptedListingResponse(user2Listing),
					);
				});
		});
		it('Should Get Disliked Listings', () => {
			return request(app.getHttpServer())
				.get(`/listings?liked=false`)
				.set({ Authorization: `Bearer ${user2Token}` })
				.expect(HttpStatus.OK)
				.expect((res) => {
					const listings: ToyListing[] = res.body.items;

					expect(listings.length).toBe(1);
					expect(listings[0]).toStrictEqual(
						exceptedListingResponse(user1Listing),
					);
				});
		});
		it('Should Get "New" Listings', () => {
			return request(app.getHttpServer())
				.get(`/listings?new`)
				.set({ Authorization: `Bearer ${user2Token}` })
				.expect(HttpStatus.OK)
				.expect((res) => {
					const listings: ToyListing[] = res.body.items;

					expect(listings.length).toBe(1);
					expect(listings[0]).toStrictEqual(
						exceptedListingResponse(user1Listing2),
					);
				});
		});
		it('Should Fail ("new" + "liked")', () => {
			return request(app.getHttpServer())
				.get(`/listings?new&liked`)
				.set({ Authorization: `Bearer ${user2Token}` })
				.expect(HttpStatus.BAD_REQUEST);
		});
		it('Should Fail (Unauthentified)', () => {
			return request(app.getHttpServer())
				.get(`/listings`)
				.expect(HttpStatus.UNAUTHORIZED);
		});
	});
	describe('Update One Listing', () => {
		it('Should Fail: Not authentified', () => {
			return request(app.getHttpServer())
				.put(`/listings/${user1Listing}`)
				.send({
					condition: 'Acceptable',
				} satisfies UpdateToyListing)
				.expect(HttpStatus.UNAUTHORIZED)
				.expect((res) => {
					expect(res.body.message).toBe('Authentication Required');
				});
		});
		it('Should Fail: Not the owner', () => {
			return request(app.getHttpServer())
				.put(`/listings/${user1Listing.id}`)
				.set({ Authorization: `Bearer ${user2Token}` })
				.send({
					condition: 'Acceptable',
				} satisfies UpdateToyListing)
				.expect(HttpStatus.UNAUTHORIZED)
				.expect((res) => {
					expect(res.body.message).toBe(
						'You can not update a listing that is not yours.',
					);
				});
		});
		it('Should Fail: Bad DTO Field Type', () => {
			return request(app.getHttpServer())
				.put(`/listings/${user1Listing.id}`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.send({
					postCode: 'AAAAAA',
				})
				.expect(HttpStatus.BAD_REQUEST);
		});
		it('Should Update A Listing (Authentified Owner (1))', () => {
			return request(app.getHttpServer())
				.put(`/listings/${user1Listing.id}`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.send({
					condition: 'Acceptable',
				} satisfies UpdateToyListing)
				.expect(HttpStatus.OK)
				.expect((res) => {
					expect(res.body).toStrictEqual({
						...user1Listing,
						condition: 'Acceptable',
					});
					user1Listing = res.body;
				});
		});
		it('Should Update A Listing (Authentified Owner (2))', () => {
			return request(app.getHttpServer())
				.put(`/listings/${user2Listing.id}`)
				.set({ Authorization: `Bearer ${user2Token}` })
				.send({
					title: 'Nintendo Switch Pro Controller White',
					type: 'VideoGame',
				} satisfies UpdateToyListing)
				.expect(HttpStatus.OK)
				.expect((res) => {
					expect(res.body).toStrictEqual({
						...user2Listing,
						title: 'Nintendo Switch Pro Controller White',
						type: 'VideoGame',
					});
					user2Listing = res.body;
				});
		});
		it('Should fail, as the listing does not exist', () => {
			return request(app.getHttpServer())
				.put(`/listings/-1`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.send({
					condition: 'Acceptable',
				} satisfies UpdateToyListing)
				.expect(HttpStatus.NOT_FOUND);
		});
	});
	describe('Delete One Listing', () => {
		it('Should Fail: Not authentified', () => {
			return request(app.getHttpServer())
				.delete(`/listings/${user1Listing.id}`)
				.expect(HttpStatus.UNAUTHORIZED)
				.expect((res) => {
					expect(res.body.message).toBe('Authentication Required');
				});
		});
		it('Should Fail: Not the owner', () => {
			return request(app.getHttpServer())
				.delete(`/listings/${user1Listing.id}`)
				.set({ Authorization: `Bearer ${user2Token}` })
				.expect(HttpStatus.UNAUTHORIZED)
				.expect((res) => {
					expect(res.body.message).toBe(
						'You can not delete a listing that is not yours.',
					);
				});
		});
		it('Should Delete A Listing (Authentified Owner)', () => {
			return request(app.getHttpServer())
				.delete(`/listings/${user1Listing.id}`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.expect(HttpStatus.OK);
		});
		it('Should fail, as the listing was deleted', () => {
			return request(app.getHttpServer())
				.get(`/listings/${user1Listing.id}`)
				.set({ Authorization: `Bearer ${user1Token}` })
				.expect(HttpStatus.NOT_FOUND)
				.expect((res) => {
					expect(res.body.message).toBe('Resource not found');
				});
		});
		it('Should fail, as the listing does not exist', () => {
			return request(app.getHttpServer())
				.delete(`/listings/-1`)
				.set({ Authorization: `Bearer ${user2Token}` })
				.expect(HttpStatus.NOT_FOUND)
				.expect((res) => {
					expect(res.body.message).toBe('Resource not found');
				});
		});
	});
});
