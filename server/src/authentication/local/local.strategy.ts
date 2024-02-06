import { Strategy } from 'passport-local';
import { PassportStrategy } from '@nestjs/passport';
import {
	HttpException,
	HttpStatus,
	Injectable,
} from '@nestjs/common';
import AuthService from '../authentication.service';
import { User } from 'src/prisma/models';

@Injectable()
export default class LocalStrategy extends PassportStrategy(Strategy) {
	constructor(private authService: AuthService) {
		super();
	}

	async validate(username: string, password: string): Promise<User> {
		return this.authService.validateUser(username, password).catch(() => {
			throw new HttpException(
				'Username or password incorrect',
				HttpStatus.FORBIDDEN,
			);
		});
	}
}
