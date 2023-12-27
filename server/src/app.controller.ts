import { Controller, Get } from '@nestjs/common';
import { ApiOperation, ApiTags } from '@nestjs/swagger';

@ApiTags('Home')
@Controller()
export class AppController {
	constructor() {}

	@ApiOperation({
		summary: 'Kinda like a health route',
	})
	@Get()
	getHello() {
		return {
			message: 'Hello World',
			date: new Date(),
		} as const;
	}
}
