import { HttpException, HttpStatus, Injectable } from '@nestjs/common';
import { JwtService } from '@nestjs/jwt';
import { User } from 'src/prisma/models';
import { UserService } from 'src/user/user.service';
import JwtPayload from './models/jwt.payload';
import JwtResponse from './models/jwt.response';

@Injectable()
export default class AuthenticationService {
	constructor(
		private userService: UserService,
		private jwtService: JwtService
	) { }

	async validateUser(username: string, plainTextPassword: string): Promise<User> {
		try {
			const requestedUser = await this.userService.getByCredentials({
				username: username,
				password: plainTextPassword
			});

			return requestedUser;
		} catch (error) {
			throw new HttpException('Username or password is incorrect', HttpStatus.NOT_FOUND);
		}
	}

	async login(user: User): Promise<JwtResponse> {
		const payload: JwtPayload = { userName: user.userName, id: user.id };

		return {
			access_token: this.jwtService.sign(payload)
		};
	}
}
