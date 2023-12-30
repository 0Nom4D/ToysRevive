import { Test, TestingModule } from '@nestjs/testing';
import { AuthenticationModule } from 'src/authentication/authentication.module';
import { PrismaModule } from 'src/prisma/prisma.module';
import { UserModule } from './user.module';
import { PrismaService } from 'src/prisma/prisma.service';
import TestPrismaService from 'test/test-prisma.service';
import SetupApp from 'test/setup-app';
import { User } from 'src/prisma/models';
import * as request from 'supertest';
import { HttpStatus, INestApplication } from '@nestjs/common';
import { CreateUserDTO } from './user.dto';

describe('UserController', () => {
	let user1: User;
	let user2: User;
	let user1Token: string;
	let app: INestApplication;

	beforeAll(async () => {
		const module: TestingModule = await Test.createTestingModule({
			imports: [AuthenticationModule, UserModule, PrismaModule],
			providers: [PrismaService],
		})
			.overrideProvider(PrismaService)
			.useClass(TestPrismaService)
			.compile();
		app = await SetupApp(module);
		await module.get(PrismaService).onModuleInit();
	});

	describe('Registration Flow', () => {
		it('Should create the admin user', () => {
			let dto: CreateUserDTO = {
				phone: '0123456789',
				email: 'user.first@user.com',
				lastName: 'First',
				firstName: 'User',
				userName: 'userfirst',
				password: '123456789',
			};
			return request(app.getHttpServer())
				.post(`/auth/register`)
				.send(dto)
				.expect(201)
				.expect((res) => {
					user1 = res.body;
					expect(user1.password).toBeUndefined();
					expect(user1.phone).toBe('0123456789');
					expect(user1.email).toBe('user.first@user.com');
					expect(user1.firstName).toBe('User');
					expect(user1.lastName).toBe('First');
					expect(user1.userName).toBe('userfirst');
					expect(user1.id).toBeDefined();
				});
		});

		it('Should create the user user', () => {
			let dto: CreateUserDTO = {
				phone: '0234567891',
				email: 'user2.first@user.com',
				lastName: 'Two',
				firstName: 'User',
				userName: 'usertwo',
				password: '12345678910',
			};
			return request(app.getHttpServer())
				.post(`/auth/register`)
				.send(dto)
				.expect(201)
				.expect((res) => {
					user2 = res.body;
					expect(user2.phone).toBe('0234567891');
					expect(user2.email).toBe('user2.first@user.com');
					expect(user2.firstName).toBe('User');
					expect(user2.lastName).toBe('Two');
					expect(user2.userName).toBe('usertwo');
					expect(user2.password).toBeUndefined();
					expect(user2.id).toBeDefined();
				});
		});

		it('Should return an error, as user already exists', () => {
			let dto: CreateUserDTO = {
				...user1,
				password: '12345678910',
			};
			return request(app.getHttpServer())
				.post(`/auth/register`)
				.send(dto)
				.expect(HttpStatus.CONFLICT)
				.expect((res) => {
					expect(res.body.message).toBeDefined();
				});
		});

		it('Should return an error, as username is not long enough', () => {
			let dto: CreateUserDTO = {
				...user1,
				userName: 'a',
				password: '12345678910',
			};
			return request(app.getHttpServer())
				.post(`/auth/register`)
				.send(dto)
				.expect(HttpStatus.BAD_REQUEST)
				.expect((res) => {
					expect(res.body.message).toBeDefined();
				});
		});

		it('Should return an error, as password is badly formed', () => {
			let dto: CreateUserDTO = {
				...user1,
				userName: 'blahblahblah',
				password: '110',
			};
			return request(app.getHttpServer())
				.post(`/auth/register`)
				.send(dto)
				.expect(HttpStatus.BAD_REQUEST)
				.expect((res) => {
					expect(res.body.message).toBeDefined();
				});
		});
	});

	describe('Authentication Flow', () => {
		it('Should Login and access their information', async () => {
			const loginResponse = await request(app.getHttpServer())
				.post(`/auth/login`)
				.send({
					username: 'userfirst',
					password: '123456789',
				});
			expect(loginResponse.statusCode).toBe(HttpStatus.CREATED);
			user1Token = loginResponse.body.access_token;
			expect(user1Token).toBeDefined();
			expect(typeof user1Token).toBe('string');

			const dataResponse = await request(app.getHttpServer())
				.get(`/users/me`)
				.set({ Authorization: `Bearer ${user1Token}` });
			expect(dataResponse.statusCode).toBe(HttpStatus.OK);
			const data = dataResponse.body;
			expect(data).toStrictEqual(user1);
			expect(data.password).toBeUndefined();
		});

		it('Should Not Be able to access route (Bad token)', async () => {
			// Here, Access token will not allow the user to be resolved
			const token = 'qqqq';
			const dataResponse = await request(app.getHttpServer())
				.get(`/users/me`)
				.set({ Authorization: `Bearer ${token}` });
			expect(dataResponse.statusCode).toBe(HttpStatus.UNAUTHORIZED);
			const data = dataResponse.body;
			expect(data.message).toBe('Bad Access Token');
		});

		it('Should Not Be able to access route (No token)', async () => {
			// Here, User is not defined
			const dataResponse = await request(app.getHttpServer()).get(
				`/users/me`,
			);
			expect(dataResponse.statusCode).toBe(HttpStatus.UNAUTHORIZED);
			const data = dataResponse.body;
			expect(data.message).toBeDefined();
		});
	});

	describe('Get Single User', () => {
		it('Should Get A User (Unauthentified)', async () => {
			const dataResponse = await request(app.getHttpServer()).get(
				`/users/${user2.id}`,
			);
			const data: User = dataResponse.body;
			expect(data.userName).toBe(user2.userName);
			expect(data.password).toBeUndefined();
			expect(data.firstName).toBeUndefined();
			expect(data.lastName).toBeUndefined();
			expect(data.email).toBeUndefined();
			expect(data.phone).toBeUndefined();
		});
		it('Should Get A User (Authentified)', async () => {
			const dataResponse = await request(app.getHttpServer())
				.get(`/users/${user2.id}`)
				.set({ Authorization: `Bearer ${user1Token}` });
			const data: User = dataResponse.body;
			expect(data.userName).toBe(user2.userName);
			expect(data.password).toBeUndefined();
			expect(data.firstName).toBeDefined();
			expect(data.email).toBeDefined();
		});
		it('Should Not Get the User (Not Found)', async () => {
			const dataResponse = await request(app.getHttpServer()).get(
				`/users/-1`,
			);
			expect(dataResponse.statusCode).toBe(HttpStatus.NOT_FOUND);
			const data = dataResponse.body;
			expect(data.message).toBeDefined();
		});
	});

	describe('Get Many Users', () => {
		it('Should Get Many Users (Unauthentified)', async () => {
			const dataResponse = await request(app.getHttpServer()).get(
				`/users`,
			);
			const data: User[] = dataResponse.body.items;
			expect(data[0].id).toEqual(user1.id);
			expect(data[0].firstName).toBeUndefined();
			expect(data[1].id).toEqual(user2.id);
			expect(data[1].phone).toBeUndefined();
		});

		it('Should Get Many Users (Authentified)', async () => {
			const dataResponse = await request(app.getHttpServer())
				.get(`/users`)
				.set({ Authorization: `Bearer ${user1Token}` });
			const data: User[] = dataResponse.body.items;
			expect(data[0].id).toEqual(user1.id);
			expect(data[0].firstName).toEqual(user1.firstName);
			expect(data[1].id).toEqual(user2.id);
			expect(data[1].phone).toBeDefined();
		});

		it('Should Get Many Users (w/ Pagination (Take))', async () => {
			const dataResponse = await request(app.getHttpServer())
				.get(`/users?take=1`)
				.set({ Authorization: `Bearer ${user1Token}` });
			const data: User[] = dataResponse.body.items;
			expect(data.length).toBe(1);
			expect(data[0].id).toEqual(user1.id);
		});
		it('Should Get Many Users (w/ Pagination (Skip))', async () => {
			const dataResponse = await request(app.getHttpServer())
				.get(`/users?skip=1`)
				.set({ Authorization: `Bearer ${user1Token}` });
			const data: User[] = dataResponse.body.items;
			expect(data.length).toBe(1);
			expect(data[0].id).toEqual(user2.id);
		});
	});
});
