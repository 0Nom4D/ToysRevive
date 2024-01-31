import { Injectable, NestMiddleware } from '@nestjs/common';
import { NextFunction, Request, Response } from 'express';

/**
 * JWT Middleware to pass the accessToken cookie to the Authorieation header
 * Must be done, as clients way not be able to pass headers to media requests (like image or audio HTML tags)
 */
@Injectable()
export class JwtCookieMiddleware implements NestMiddleware {
	use(req: Request, _res: Response, next: NextFunction) {
		if (req.cookies && req.cookies['accessToken'] !== undefined) {
			req.headers.authorization ??= `Bearer ${req.cookies['accessToken']}`;
		}
		next();
	}
}

export default JwtCookieMiddleware;
