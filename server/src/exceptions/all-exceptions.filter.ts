import {
	ArgumentsHost,
	Catch,
	HttpException,
	HttpStatus,
	Logger,
} from '@nestjs/common';
import { BaseExceptionFilter } from '@nestjs/core';
import type { Response } from 'express';

@Catch()
export default class AllExceptionsFilter extends BaseExceptionFilter {
	catch(exception: Error, host: ArgumentsHost) {
		const logger = new Logger();
		const ctx = host.switchToHttp();
		const response = ctx.getResponse<Response>();

		if (exception instanceof HttpException) {
			const httpException = exception as HttpException;

			response.status(httpException.getStatus()).json({
				statusCode: httpException.getStatus(),
				message: httpException.message,
			});
		} else {
			logger.error(exception);
			response.status(HttpStatus.INTERNAL_SERVER_ERROR).json({
				statusCode: HttpStatus.INTERNAL_SERVER_ERROR,
				message: 'An error occured',
			});
		}
	}
}
