import { Controller, Get, Param, Post, Request, UseGuards } from '@nestjs/common';
import { UpdateUserDTO } from './user.dto';
import { AuthGuard } from '@nestjs/passport';
import { User } from 'src/prisma/models';
import JwtAuthGuard from 'src/authentication/jwt/jwt-auth.guard';

@Controller('users')
export class UserController {
	
	@Get()
	public getUsers() {

	}

	@Get(':id')
	public getUser(
		@Param('id') id: number
	) {
		

	}

	@Get('me')
	@UseGuards(JwtAuthGuard)
	public getCurrentUser(@Request() req: Express.Request) {
		return (req as { user: User }).user;
	}

	@Post('me')
	@UseGuards(JwtAuthGuard)
	public updateCurrentUser(
		updateDTO: UpdateUserDTO
	) {

	}
}
