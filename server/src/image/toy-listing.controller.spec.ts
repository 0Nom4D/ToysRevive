import { Test, TestingModule } from '@nestjs/testing';
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
import { ToyListingModule } from 'src/toy-listing/toy-listing.module';

describe('Image Controller', () => {
	let user1: User;
	let user1Listing: ToyListing;
	let user2: User;
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
		).body.access_token;
		user2Token = (
			await request(app.getHttpServer()).post(`/auth/login`).send({
				username: 'secondLast',
				password: 'MyPassword2',
			})
		).body.access_token;
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
	});
});
