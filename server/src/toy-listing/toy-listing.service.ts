import { Injectable } from '@nestjs/common';
import { Prisma, ToyListing } from '@prisma/client';
import { PaginationParameters } from 'src/pagination/models/pagination-parameters';
import { CreateToyListing } from 'src/prisma/models';
import { PrismaService } from 'src/prisma/prisma.service';

@Injectable()
export class ToyListingService {
	constructor(private prismaService: PrismaService) {}

	async getMany(
		where: Partial<Omit<ToyListing, 'id'>>,
		pagination: PaginationParameters,
		sortBy?: { sortBy: Prisma.ToyListingScalarFieldEnum, order: Prisma.SortOrder }
	) {
		return this.prismaService.toyListing.findMany({
			where: where,
			orderBy: sortBy ? { [sortBy.sortBy]: sortBy.order } : {},
			take: pagination.take,
			skip: pagination.skip,
		});
	}

	async get(id: number) {
		return this.prismaService.toyListing.findFirstOrThrow({
			where: { id },
		});
	}

	async create(create: CreateToyListing, ownerId: number) {
		return this.prismaService.toyListing.create({
			data: { ...create, ownerId },
		});
	}

	async delete(id: number) {
		return this.prismaService.toyListing.delete({ where: { id } });
	}

	async update(id: number, data: Partial<Omit<ToyListing, 'id'>>) {
		return this.prismaService.toyListing.update({ where: { id }, data });
	}
}
