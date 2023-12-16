import {
	ArgumentsHost, Catch, HttpStatus, Logger
} from '@nestjs/common';
import { BaseExceptionFilter } from '@nestjs/core';
import { Prisma } from '@prisma/client';
import type { Response } from 'express';
import { PrismaError } from 'prisma-error-enum';

@Catch()
export default class PrismaExceptionsFilter extends BaseExceptionFilter {
	catch(exception: any, host: ArgumentsHost) {
		const logger = new Logger();
		const ctx = host.switchToHttp();
		const response = ctx.getResponse<Response>();
		
		if (exception instanceof Prisma.PrismaClientKnownRequestError == false) {
			return;
		}
		const body = this.prismaErrorToHttpResponse(exception)

		if (body) {
			response
				.status(body.code)
				.json({
					statusCode: body.code,
					message: body.message
				});
		}		
	}

	private prismaErrorToHttpResponse(error: Prisma.PrismaClientKnownRequestError) {
		switch (error.code) {
			case PrismaError.RecordsNotFound:
				return { message: 'Resource not found', code: HttpStatus.NOT_FOUND };
			default:
				return;
		}
	}
}
