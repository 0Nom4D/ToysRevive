/* eslint-disable array-bracket-newline */
// Collections of Interceptors, Pipes & Filters for the global app

import {
	ClassSerializerInterceptor,
	HttpException,
	HttpStatus,
	INestApplication,
	MiddlewareConsumer,
	RequestMethod,
	ValidationPipe,
} from '@nestjs/common';
import { HttpAdapterHost, Reflector } from '@nestjs/core';
import * as cookieParser from 'cookie-parser';
import helmet from 'helmet';
import { JwtCookieMiddleware } from './authentication/jwt/jwt-middleware';
import AllExceptionsFilter from './exceptions/all-exceptions.filter';
import PrismaExceptionsFilter from './exceptions/prisma.filter';
import NotFoundExceptionFilter from './exceptions/not-found.exception';

// To call before application bootstrap/launch
const presetup = () => {};

// Interceptors to use
const buildInterceptors = (app: INestApplication) => [
	new ClassSerializerInterceptor(app.get(Reflector)),
];

const buildExceptionFilters = (app: INestApplication) => [
	new AllExceptionsFilter(app.get(HttpAdapterHost)),
	new NotFoundExceptionFilter(),
	new PrismaExceptionsFilter(),
];

const buildPipes = (_app: INestApplication) => [
	new ValidationPipe({
		transform: true,
		exceptionFactory: (error) => {
			const failedConstraint = Object.keys(error[0].constraints!)[0];

			return new HttpException(
				error[0].constraints![failedConstraint],
				HttpStatus.BAD_REQUEST,
			);
		},
		whitelist: true,
		transformOptions: {
			enableImplicitConversion: true,
		},
	}),
];

const buildHttpPlugs = (_app: INestApplication) => [
	helmet({
		crossOriginResourcePolicy:
			process.env.NODE_ENV === 'development'
				? { policy: 'cross-origin' }
				: true,
	}),
	cookieParser(),
];

const applyMiddlewares = (consumer: MiddlewareConsumer) =>
	consumer
		.apply(JwtCookieMiddleware)
		.forRoutes({ path: '*', method: RequestMethod.ALL });

export {
	presetup,
	buildInterceptors,
	applyMiddlewares,
	buildPipes,
	buildExceptionFilters,
	buildHttpPlugs,
};
