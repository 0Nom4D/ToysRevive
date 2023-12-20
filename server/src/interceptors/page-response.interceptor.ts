import {
	CallHandler,
	ExecutionContext,
	Injectable,
	NestInterceptor,
} from '@nestjs/common';
import PaginatedResponse from 'src/pagination/models/paginated-response';
import { map } from 'rxjs';

/**
 * Interceptor that catches a array of items, and turns it into a page of items
 */
@Injectable()
export default class PaginatedResponseBuilderInterceptor
	implements NestInterceptor
{
	intercept(context: ExecutionContext, next: CallHandler<any>) {
		const request = context.switchToHttp().getRequest();

		return next
			.handle()
			.pipe(map((items) => new PaginatedResponse(items, request)));
	}
}
