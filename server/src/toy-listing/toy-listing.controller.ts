import {
	Body,
	Controller,
	Delete,
	Get,
	HttpException,
	HttpStatus,
	Param,
	Post,
	Query,
	Request,
	UseGuards,
	UseInterceptors,
} from '@nestjs/common';
import { ApiOperation, ApiPropertyOptional, ApiTags } from '@nestjs/swagger';
import { Condition, Prisma, ToyType } from '@prisma/client';
import JwtAuthGuard from 'src/authentication/jwt/jwt-auth.guard';
import PaginatedResponseBuilderInterceptor from 'src/interceptors/page-response.interceptor';
import { PaginationParameters } from 'src/pagination/models/pagination-parameters';
import { ApiPaginatedResponse } from 'src/pagination/paginated-response.decorator';
import { CreateToyListing, ToyListing, UpdateToyListing } from 'src/prisma/models';
import { ToyListingService } from './toy-listing.service';

class QueryParameters {
	@ApiPropertyOptional({ description: 'Filter by Condition', enum: Condition })
	condition?: Condition;
	@ApiPropertyOptional({ description: 'Filter by Toy Type', enum: ToyType })
	type?: ToyType;
	@ApiPropertyOptional({ description: 'Filter by Post Code' })
	postCode?: number;
	@ApiPropertyOptional({ description: 'Filter by Owner' })
	ownerId?: number;
}

class SortParameters {
	@ApiPropertyOptional({ description: 'Sorting Field', enum: Prisma.ToyListingScalarFieldEnum })
	sortBy?: Prisma.ToyListingScalarFieldEnum;
	@ApiPropertyOptional({ description: 'Sorting Order', enum: Prisma.SortOrder })
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
	@ApiPaginatedResponse(ToyListing)
	@UseInterceptors(PaginatedResponseBuilderInterceptor)
	public getListings(
		@Query()
		paginationParameters?: PaginationParameters,
		@Query() selector?: QueryParameters,
		@Query() sort?: SortParameters
		
	) {
		return this.toyListingService.getMany(
			selector ?? {},
			paginationParameters,
			{ sortBy: sort.sortBy ?? 'addDate', order: sort.order ?? 'desc' }
		);
	}

	@Get(':id')
	@ApiOperation({
		summary: 'Get A Single Listing',
	})
	public getListing(@Param('id') id: number): Promise<ToyListing> {
		return this.toyListingService.get(id);
	}

	@Post(':id')
	@ApiOperation({
		summary: 'Update Listing',
	})
	@UseGuards(JwtAuthGuard)
	public async updateListing(
		@Param('id') id: number,
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

	@Delete(':id')
	@ApiOperation({
		summary: 'Delete Listing',
	})
	@UseGuards(JwtAuthGuard)
	public async deleteListing(@Param('id') id: number, @Request() req: any): Promise<ToyListing> {
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
	@ApiOperation({
		summary: 'Create Listing',
	})
	@UseGuards(JwtAuthGuard)
	public createListing(
		@Body() createDto: CreateToyListing,
		@Request() req: any
	): Promise<ToyListing> {
		const authedUserId = req.user.id;

		return this.toyListingService.create(createDto, authedUserId);
	}
}
