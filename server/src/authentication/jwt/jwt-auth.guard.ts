
import { ExecutionContext, Injectable } from '@nestjs/common';
import { Reflector } from '@nestjs/core';
import { AuthGuard } from '@nestjs/passport';

@Injectable()
export default class JwtAuthGuard extends AuthGuard('jwt') {
	constructor(private reflector: Reflector) {
		super();
	}

	handleRequest(err: any, user: any, _info: any, _context: ExecutionContext, _status?: any): any {
		if (err || !user) {
			throw err;
		}
		return user;
	}
}
