import {
	Body,
	Controller,
	Delete,
	Get,
	HttpException,
	HttpStatus,
	Param,
	ParseIntPipe,
	Post,
	Put,
	Query,
	Request,
	UseGuards,
	UseInterceptors,
} from '@nestjs/common';
import {
	ApiBearerAuth,
	ApiOperation,
	ApiPropertyOptional,
	ApiTags,
} from '@nestjs/swagger';
import { Condition, Prisma, ToyType } from '@prisma/client';
import JwtAuthGuard from 'src/authentication/jwt/jwt-auth.guard';
import PaginatedResponseBuilderInterceptor from 'src/interceptors/page-response.interceptor';
import { PaginationParameters } from 'src/pagination/models/pagination-parameters';
import { ApiPaginatedResponse } from 'src/pagination/paginated-response.decorator';
import {
	CreateToyListing,
	ToyListing,
	UpdateToyListing,
} from 'src/prisma/models';
import { ToyListingService } from './toy-listing.service';
import { IsBoolean, IsEnum, IsNumber, IsOptional } from 'class-validator';
import { ToyListingResponse } from './toy-listing.response';
import { Transform } from 'class-transformer';

class QueryParameters {
	@ApiPropertyOptional({
		description: 'Filter by Condition',
		enum: Condition,
	})
	@IsEnum(Condition)
	@IsOptional()
	condition?: Condition;
	@ApiPropertyOptional({
		description:
			'If specified, will get listings that do NOT belong to the user, that they have dis/liked',
	})
	@IsOptional()
	@Transform(({ obj }) => obj.liked === 'true')
	liked?: boolean;
	@ApiPropertyOptional({
		description:
			'If specified, will get listings that do NOT belong to the user, that they have not dis/liked',
	})
	@IsOptional()
	@IsBoolean()
	new?: true;
	@ApiPropertyOptional({ description: 'Filter by Toy Type', enum: ToyType })
	@IsEnum(ToyType)
	@IsOptional()
	type?: ToyType;
	@ApiPropertyOptional({ description: 'Filter by Post Code' })
	@IsNumber()
	@IsOptional()
	postCode?: number;
	@ApiPropertyOptional({ description: 'Filter by Owner' })
	@IsNumber()
	@IsOptional()
	ownerId?: number;
}

class SortParameters {
	@ApiPropertyOptional({
		description: 'Sorting Field',
		enum: Prisma.ToyListingScalarFieldEnum,
	})
	@IsEnum(Prisma.ToyListingScalarFieldEnum)
	@IsOptional()
	sortBy?: Prisma.ToyListingScalarFieldEnum;
	@ApiPropertyOptional({
		description: 'Sorting Order',
		enum: Prisma.SortOrder,
	})
	@IsEnum(Prisma.SortOrder)
	@IsOptional()
	order?: 'asc' | 'desc';
}

@ApiTags('Toy Listings')
@Controller('listings')
export class ToyListingController {
	constructor(private toyListingService: ToyListingService) {}

	@Get()
	@ApiOperation({
		summary: 'Get Many Listings',
	})
	@ApiBearerAuth()
	@UseGuards(JwtAuthGuard)
	@ApiPaginatedResponse(ToyListingResponse)
	@UseInterceptors(PaginatedResponseBuilderInterceptor)
	public getListings(
		@Request() req: any,
		@Query()
		paginationParameters?: PaginationParameters,
		@Query() selector?: QueryParameters,
		@Query() sort?: SortParameters,
	) {
		if (selector.liked !== undefined && selector.new !== undefined) {
			throw new HttpException(
				"'liked' and 'new' can not be mutually used",
				HttpStatus.BAD_REQUEST,
			);
		}
		const { liked, new: __, ...otherArgs } = selector;
		let where: Prisma.ToyListingWhereInput = otherArgs;

		if (selector.new !== undefined || selector.liked !== undefined) {
			where = {
				...where,
				AND: [
					{
						ownerId: {
							not: req.user.id,
						},
					},
					{
						ownerId: selector.ownerId,
					},
				],
				likedBy: {
					some:
						selector.liked !== undefined
							? {
									liked: selector.liked,
									userId: req.user.id,
								}
							: undefined,
					none:
						selector.new !== undefined
							? {
									userId: req.user.id,
								}
							: undefined,
				},
			} satisfies Prisma.ToyListingWhereInput;
		}
		return this.toyListingService.getMany(
			where ?? {},
			paginationParameters,
			{
				sortBy: sort.sortBy ?? 'addDate',
				order: sort.order ?? 'desc',
			},
		);
	}

	@Get(':id')
	@ApiOperation({
		summary: 'Get A Single Listing',
	})
	@ApiBearerAuth()
	@UseGuards(JwtAuthGuard)
	public getListing(
		@Param('id', ParseIntPipe) id: number,
	): Promise<ToyListingResponse> {
		return this.toyListingService.get(id);
	}

	@Put(':id')
	@ApiBearerAuth()
	@ApiOperation({
		summary: 'Update Listing',
	})
	@UseGuards(JwtAuthGuard)
	public async updateListing(
		@Param('id', ParseIntPipe) id: number,
		@Body() updateDto: UpdateToyListing,
		@Request() req: any,
	): Promise<ToyListing> {
		const listing = await this.toyListingService.get(id);
		const authedUserId = req.user.id;

		if (listing.ownerId !== authedUserId) {
			throw new HttpException(
				'You can not update a listing that is not yours.',
				HttpStatus.UNAUTHORIZED,
			);
		}
		return this.toyListingService.update(listing.id, updateDto);
	}

	@Post(':id/like')
	@ApiBearerAuth()
	@ApiOperation({
		summary: 'Set a Listing as "liked"',
	})
	@UseGuards(JwtAuthGuard)
	public async likeListing(
		@Param('id', ParseIntPipe) id: number,
		@Request() req: any,
	): Promise<void> {
		await this.saveLikeListing(req.user.id, id, true);
	}

	@Post(':id/dislike')
	@ApiBearerAuth()
	@ApiOperation({
		summary: 'Set a Listing as "disliked"',
	})
	@UseGuards(JwtAuthGuard)
	public async dislikeListing(
		@Param('id', ParseIntPipe) id: number,
		@Request() req: any,
	): Promise<void> {
		await this.saveLikeListing(req.user.id, id, false);
	}

	private async saveLikeListing(
		authedUserId: number,
		listingId: number,
		like: boolean,
	) {
		const listing = await this.toyListingService.get(listingId);

		if (listing.ownerId === authedUserId) {
			throw new HttpException(
				'You can not (dis)like a listing that is not yours.',
				HttpStatus.BAD_REQUEST,
			);
		}
		await this.toyListingService.likeListing(
			authedUserId,
			listing.id,
			like,
		);
	}

	@Delete(':id')
	@ApiBearerAuth()
	@ApiOperation({
		summary: 'Delete Listing',
	})
	@UseGuards(JwtAuthGuard)
	public async deleteListing(
		@Param('id', ParseIntPipe) id: number,
		@Request() req: any,
	): Promise<ToyListing> {
		const listing = await this.toyListingService.get(id);
		const authedUserId = req.user.id;

		if (listing.ownerId !== authedUserId) {
			throw new HttpException(
				'You can not delete a listing that is not yours.',
				HttpStatus.UNAUTHORIZED,
			);
		}
		return this.toyListingService.delete(listing.id);
	}

	@Post()
	@ApiBearerAuth()
	@ApiOperation({
		summary: 'Create Listing',
	})
	@UseGuards(JwtAuthGuard)
	public createListing(
		@Body() createDto: CreateToyListing,
		@Request() req: any,
	): Promise<ToyListing> {
		const authedUserId = req.user.id;

		return this.toyListingService.create(createDto, authedUserId);
	}
}
