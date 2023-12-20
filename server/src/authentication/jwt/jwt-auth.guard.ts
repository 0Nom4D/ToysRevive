
import { ExecutionContext, HttpException, HttpStatus, Injectable } from '@nestjs/common';
import { Reflector } from '@nestjs/core';
import { JsonWebTokenError } from '@nestjs/jwt';
import { AuthGuard } from '@nestjs/passport';

@Injectable()
export default class JwtAuthGuard extends AuthGuard('jwt') {
	constructor(private reflector: Reflector) {
		super();
	}

	handleRequest(err: any, user: any, info: any, _context: ExecutionContext, _status?: any): any {
		if (info instanceof JsonWebTokenError) {
			throw new HttpException('Bad Access Token', HttpStatus.UNAUTHORIZED);
		}
		if (err || !user) {
			throw err || new HttpException('Authentication Required', HttpStatus.UNAUTHORIZED);
		}
		return user;
	}

	canActivate(context: ExecutionContext) {
		return super.canActivate(context);
	}

}

@Injectable()
export class OptionalJwtAuthGuard extends JwtAuthGuard {

	handleRequest(err: any, user: any, info: any, _context: ExecutionContext, _status?: any): any {
		try {
			return super.handleRequest(err, user, info, _context, _status);
		} catch {
			return null;
		}
	}

	canActivate(context: ExecutionContext) {
		return super.canActivate(context);
	}
}
