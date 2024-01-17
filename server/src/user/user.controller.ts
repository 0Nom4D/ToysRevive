import {
	Controller,
	Get,
	Param,
	ParseIntPipe,
	Query,
	Request,
	UseGuards,
	UseInterceptors,
} from '@nestjs/common';
import JwtAuthGuard, {
	OptionalJwtAuthGuard,
} from 'src/authentication/jwt/jwt-auth.guard';
import { AuthedUserResponse, PublicUserResponse } from './user.response';
import { UserService } from './user.service';
import { PaginationParameters } from 'src/pagination/models/pagination-parameters';
import { User } from '@prisma/client';
import { ApiPaginatedResponse } from 'src/pagination/paginated-response.decorator';
import PaginatedResponseBuilderInterceptor from 'src/interceptors/page-response.interceptor';
import { ApiBearerAuth, ApiOperation, ApiTags } from '@nestjs/swagger';

@ApiTags('Users')
@Controller('users')
export class UserController {
	constructor(private userService: UserService) {}

	@Get()
	@ApiOperation({
		summary: 'Get Many Users',
	})
	@UseGuards(OptionalJwtAuthGuard)
	@ApiPaginatedResponse(AuthedUserResponse)
	@UseInterceptors(PaginatedResponseBuilderInterceptor)
	public getUsers(
		@Request() req: any,
		@Query()
		paginationParameters: PaginationParameters,
	) {
		return this.userService.getMany(paginationParameters).then((users) =>
			users.map((user) => {
				return this.filterUserMember(user, req);
			}),
		);
	}

	@Get('me')
	@ApiBearerAuth()
	@ApiOperation({
		summary: 'Get Profile of the currently authentified user',
	})
	@UseGuards(JwtAuthGuard)
	public async getCurrentUser(@Request() req: any) {
		const userId: number = req.user.id;

		return new AuthedUserResponse(await this.userService.getById(userId));
	}

	@Get(':id')
	@ApiOperation({
		summary: 'Get A Profile from a User ID',
	})
	@UseGuards(OptionalJwtAuthGuard)
	public async getUser(
		@Param('id', ParseIntPipe) id: number,
		@Request() req: any,
	): Promise<PublicUserResponse> {
		const user = await this.userService.getById(id);

		return this.filterUserMember(user, req);
	}

	private filterUserMember(user: User, request: any) {
		const isLoggedin = request.user != null;

		if (isLoggedin) {
			return new AuthedUserResponse(user);
		}
		return new PublicUserResponse(user);
	}
}
