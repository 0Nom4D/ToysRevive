import {
	Controller,
	Get,
	Param,
	Post,
	Request,
	UseGuards,
} from '@nestjs/common';
import { UpdateUserDTO } from './user.dto';
import JwtAuthGuard, {
	OptionalJwtAuthGuard,
} from 'src/authentication/jwt/jwt-auth.guard';
import { AuthedUserResponse, PublicUserResponse } from './user.response';
import { UserService } from './user.service';

@Controller('users')
export class UserController {
	constructor(private userService: UserService) {}

	@Get()
	public getUsers() {}

	@Get('me')
	@UseGuards(JwtAuthGuard)
	public async getCurrentUser(@Request() req: any) {
		const userId: number = req.user.id;

		return new AuthedUserResponse(await this.userService.getById(userId));
	}

	@Post('me')
	@UseGuards(JwtAuthGuard)
	public updateCurrentUser(updateDTO: UpdateUserDTO) {}

	@Get(':id')
	@UseGuards(OptionalJwtAuthGuard)
	public async getUser(@Param('id') id: number, @Request() req: any) {
		const user = await this.userService.getById(id);
		const isLoggedin = req.user != null;

		if (isLoggedin) {
			return new AuthedUserResponse(user);
		}
		return new PublicUserResponse(user);
	}
}
