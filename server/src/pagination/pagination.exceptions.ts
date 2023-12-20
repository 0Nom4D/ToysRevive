import { HttpException, HttpStatus } from '@nestjs/common';
import type { PaginationParameters } from './models/pagination-parameters';

export default class InvalidPaginationParameterValue extends HttpException {
	constructor(key: keyof PaginationParameters) {
		super(
			`Invalid '${key}' parameter: expected positive integer`,
			HttpStatus.BAD_REQUEST,
		);
	}
}
