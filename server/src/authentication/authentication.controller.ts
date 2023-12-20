import AuthenticationService from './authentication.service';
import {
	Body,
	Controller, Post, Request, UseGuards
} from '@nestjs/common';
import * as Express from 'express';
import { LocalAuthGuard } from './local/local-auth.guard';
import {
	ApiBody, ApiOperation, ApiTags
} from '@nestjs/swagger';
import { User } from 'src/prisma/models';
import LoginDTO from './models/login.dto';
import { UserService } from 'src/user/user.service';
import { CreateUserDTO } from 'src/user/user.dto';
import { AuthedUserResponse } from 'src/user/user.response';

@ApiTags('Authentication')
@Controller('auth')
export default class AuthenticationController {
	constructor(
		private userService: UserService,
		private authenticationService: AuthenticationService,
	) {}

	@ApiOperation({
		summary: 'Login user',
	})
	@ApiBody({
		type: LoginDTO
	})
	@UseGuards(LocalAuthGuard)
	@Post('login')
	async login(@Request() request: Express.Request) {
		return this.authenticationService.login(
			(request as unknown as { user: User }).user
		);
	}

	@ApiOperation({
		summary: 'Register new user',
	})
	@ApiBody({
		type: CreateUserDTO
	})
	@Post('register')
	async register(@Body() dto: CreateUserDTO): Promise<AuthedUserResponse> {
		const createdUser = await this.userService.createUser(dto);

		return new AuthedUserResponse(createdUser);
	}
}
