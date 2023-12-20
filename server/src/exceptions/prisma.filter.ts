import {
	ArgumentsHost, Catch, HttpStatus
} from '@nestjs/common';
import { BaseExceptionFilter } from '@nestjs/core';
import { Prisma } from '@prisma/client';
import type { Response } from 'express';
import { PrismaError } from 'prisma-error-enum';

@Catch(Prisma.PrismaClientKnownRequestError)
export default class PrismaExceptionsFilter extends BaseExceptionFilter {
	catch(exception: Prisma.PrismaClientKnownRequestError, host: ArgumentsHost) {
		const ctx = host.switchToHttp();
		const response = ctx.getResponse<Response>();

		try {
			const body = this.prismaErrorToHttpResponse(exception)
	
			response
				.status(body.code)
				.json({
					statusCode: body.code,
					message: body.message
				});
		} catch {
			response
			.status(HttpStatus.INTERNAL_SERVER_ERROR)
			.json({
				statusCode: HttpStatus.INTERNAL_SERVER_ERROR,
				message: `Uncaught ORM Error. Code ${exception.code}`
			});
		}
	}

	private prismaErrorToHttpResponse(error: Prisma.PrismaClientKnownRequestError) {
		switch (error.code) {
			case PrismaError.UniqueConstraintViolation:
				return { message: 'Resource already exists', code: HttpStatus.CONFLICT };
			case PrismaError.RecordsNotFound:
				return { message: 'Resource not found', code: HttpStatus.NOT_FOUND };
			default:
				throw new Error("Uncaught ORM Error");
		}
	}
}
