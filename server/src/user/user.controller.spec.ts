import { Test, TestingModule } from '@nestjs/testing';
import { UserController } from './user.controller';
import { AuthenticationModule } from 'src/authentication/authentication.module';
import { PrismaModule } from 'src/prisma/prisma.module';
import { UserModule } from './user.module';
import { PrismaService } from 'src/prisma/prisma.service';
import TestPrismaService from 'test/test-prisma.service';
import SetupApp from 'test/setup-app';
import { User } from 'src/prisma/models';
import * as request from "supertest";
import { HttpStatus, INestApplication } from '@nestjs/common';
import { CreateUserDTO } from './user.dto';
import { AppProviders } from 'src/app.plugins';

describe('UserController', () => {
	let user1: User;
	let user2: User;
	let user1Token: string;
	let app: INestApplication;

	beforeAll(async () => {
		const module: TestingModule = await Test.createTestingModule({
			imports: [PrismaModule, AuthenticationModule, UserModule],
			providers: [PrismaService, ...AppProviders],
		})
			.overrideProvider(PrismaService)
			.useClass(TestPrismaService)
			.compile();
		app = await SetupApp(module);
		await module.get(PrismaService).onModuleInit();
	});

	describe('Registration Flow', () => {
		it("Should create the admin user", () => {
			let dto: CreateUserDTO = {
				phone: '+33 123456789',
				email: 'user.first@user.com',
				lastName: 'First',
				firstName: 'User',
				userName: 'userfirst',
				password: '123456789'
			}
			return request(app.getHttpServer())
				.post(`/auth/register`)
				.send(dto)
				.expect(201)
				.expect((res) => {
					user1 = res.body;
					expect(user1.phone).toBe('+33 123456789');
					expect(user1.email).toBe('user.first@user.com');
					expect(user1.firstName).toBe('User');
					expect(user1.lastName).toBe('First');
					expect(user1.userName).toBe('userfirst');
					expect(user1.password).toBeUndefined();
					expect(user1.id).toBeDefined();

				});
		});

		it("Should create the user user", () => {
			let dto: CreateUserDTO = {
				phone: '+33 234567891',
				email: 'user2.first@user.com',
				lastName: 'Two',
				firstName: 'User',
				userName: 'usertwo',
				password: '12345678910'
			}
			return request(app.getHttpServer())
				.post(`/auth/register`)
				.send(dto)
				.expect(201)
				.expect((res) => {
					user2 = res.body;
					expect(user2.phone).toBe('+33 234567891');
					expect(user2.email).toBe('user.two@user.com');
					expect(user2.firstName).toBe('User');
					expect(user2.lastName).toBe('Two');
					expect(user2.userName).toBe('usertwo');
					expect(user2.password).toBeUndefined();
					expect(user2.id).toBeDefined();

				});
		});

		it("Should return an error, as user already exists", () => {
			let dto: CreateUserDTO = {
				...user1,
				password: '12345678910'
			}
			return request(app.getHttpServer())
				.post(`/auth/register`)
				.send(dto)
				.expect(HttpStatus.CONFLICT)
				.expect((res) => {
					expect(res.body.message).toBeDefined();
				});
		});

		it("Should return an error, as username is not long enough", () => {
			let dto: CreateUserDTO = {
				...user1,
				userName: 'a',
				password: '12345678910'
			}
			return request(app.getHttpServer())
				.post(`/auth/register`)
				.send(dto)
				.expect(HttpStatus.BAD_REQUEST)
				.expect((res) => {
					expect(res.body.message).toBeDefined();
				});
		});

		it("Should return an error, as password is badly formed", () => {
			let dto: CreateUserDTO = {
				...user1,
				userName: 'blahblahblah',
				password: '110'
			}
			return request(app.getHttpServer())
				.post(`/auth/register`)
				.send(dto)
				.expect(HttpStatus.BAD_REQUEST)
				.expect((res) => {
					expect(res.body.message).toBeDefined();
				});
		});
	})

	describe('Authentication Flow', () => {
		it('Should Login and access their information', async () => {
			const loginResponse = await request(app.getHttpServer())
				.post(`/auth/login`)
				.send({
					username: user1.userName,
					password: '123456789'
				})
			expect(loginResponse.statusCode).toBe(HttpStatus.OK);
			user1Token = loginResponse.body.access_token;
			expect(user1Token).toBeDefined();
			expect(typeof user1Token).toBe('string');

			const dataResponse = await request(app.getHttpServer())
				.post(`/users/me`)
				.set('cookie', `access_token=${user1Token}`)
			expect(dataResponse.statusCode).toBe(HttpStatus.OK);
			const data = dataResponse.body;
			expect(data).toStrictEqual(user1);
			expect(data.password).toBeUndefined();
		})

		it('Should Not Be able to access route (Bad token)', async () => {
			// Here, Access token will not allow the user to be resolved
			const token = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyTmFtZSI6InVzZXIxIiwiaWQiOjEwMDAsImlhdCI6MTcwMTc2Njc1MSwiZXhwIjoxNzEwNDA2NzUxfQ.Vrms8idLpLX7Q0wZnCeeWicRzCc4TYOaoV2D9ZGDplI'
			const dataResponse = await request(app.getHttpServer())
				.post(`/users/me`)
				.set('cookie', `access_token=${token}`)
			expect(dataResponse.statusCode).toBe(HttpStatus.BAD_REQUEST);
			const data = dataResponse.body;
			expect(data.message).toBe('Bad Access Token');
		})

		it('Should Not Be able to access route (No token)', async () => {
			// Here, User is not defined
			const dataResponse = await request(app.getHttpServer())
				.post(`/users/me`)
			expect(dataResponse.statusCode).toBe(HttpStatus.UNAUTHORIZED);
			const data = dataResponse.body;
			expect(data.message).toBeDefined();
		})
	})

	describe('Get Single User', () => {
		it('Should Get A User (Unauthentified)', async () => {
			const dataResponse = await request(app.getHttpServer())
				.post(`/users/${user2.id}`)
				.set('cookie', `access_token=${user1Token}`)
			const data: User = dataResponse.body;
			expect(data.userName).toBe(user1.userName);
			expect(data.password).toBeUndefined();
			expect(data.firstName).toBeUndefined();
			expect(data.lastName).toBeUndefined();
			expect(data.email).toBeUndefined();
			expect(data.phone).toBeUndefined();
		})
		it('Should Get A User (Authentified)', async () => {
			const dataResponse = await request(app.getHttpServer())
				.post(`/users/${user2.id}`)
				.set('cookie', `access_token=${user1Token}`)
			const data: User = dataResponse.body;
			expect(data.userName).toBe(user1.userName);
			expect(data.password).toBeUndefined();
			expect(data.firstName).toBeDefined();
			expect(data.email).toBeDefined();
		})
		it('Should Not Get the User (Not Found)', async () => {
			const dataResponse = await request(app.getHttpServer())
				.post(`/users/-1`)
			expect(dataResponse.statusCode).toBe(HttpStatus.NOT_FOUND);
			const data = dataResponse.body;
			expect(data.message).toBeDefined();
		})
	})
});
