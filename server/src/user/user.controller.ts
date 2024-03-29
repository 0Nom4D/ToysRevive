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
import JwtAuthGuard from 'src/authentication/jwt/jwt-auth.guard';
import { UserResponse } from './user.response';
import { UserService } from './user.service';
import { PaginationParameters } from 'src/pagination/models/pagination-parameters';
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
	@ApiBearerAuth()
	@UseGuards(JwtAuthGuard)
	@ApiPaginatedResponse(UserResponse)
	@UseInterceptors(PaginatedResponseBuilderInterceptor)
	public getUsers(
		@Query()
		paginationParameters: PaginationParameters,
	) {
		return this.userService.getMany(paginationParameters).then((users) =>
			users.map((user) => {
				return new UserResponse(user);
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

		return new UserResponse(await this.userService.getById(userId));
	}

	@Get(':id')
	@ApiOperation({
		summary: 'Get A Profile from a User ID',
	})
	@ApiBearerAuth()
	@UseGuards(JwtAuthGuard)
	public async getUser(
		@Param('id', ParseIntPipe) id: number,
	): Promise<UserResponse> {
		const user = await this.userService.getById(id);

		return new UserResponse(user);
	}
}
