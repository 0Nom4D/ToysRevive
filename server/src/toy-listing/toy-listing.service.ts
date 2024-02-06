import { Injectable } from '@nestjs/common';
import { Prisma, ToyListing } from '@prisma/client';
import { ImageService } from 'src/image/image.service';
import { PaginationParameters } from 'src/pagination/models/pagination-parameters';
import { CreateToyListing } from 'src/prisma/models';
import { PrismaService } from 'src/prisma/prisma.service';

@Injectable()
export class ToyListingService {
	constructor(
		private prismaService: PrismaService,
		protected imageService: ImageService,
	) {}

	async getMany(
		where: Partial<Omit<Prisma.ToyListingWhereInput, 'id'>>,
		pagination: PaginationParameters,
		sortBy?: {
			sortBy: Prisma.ToyListingScalarFieldEnum;
			order: Prisma.SortOrder;
		},
	) {
		return this.prismaService.toyListing.findMany({
			where: where,
			orderBy: sortBy ? { [sortBy.sortBy]: sortBy.order } : {},
			take: pagination.take,
			skip: pagination.skip,
			include: { images: true },
		});
	}

	async get(id: number) {
		return this.prismaService.toyListing.findFirstOrThrow({
			where: { id },
			include: { images: true },
		});
	}

	async create(create: CreateToyListing, ownerId: number) {
		return this.prismaService.toyListing.create({
			data: { ...create, ownerId },
		});
	}

	async delete(id: number) {
		const listing = await this.prismaService.toyListing.findFirstOrThrow({
			where: { id },
			include: { images: true },
		});

		await Promise.all(
			listing.images.map((image) =>
				this.imageService.deleteImage(image.id),
			),
		);
		return this.prismaService.toyListing.delete({ where: { id } });
	}

	async update(id: number, data: Partial<Omit<ToyListing, 'id'>>) {
		return this.prismaService.toyListing.update({ where: { id }, data });
	}

	async getParentListing(imageId: number) {
		return this.prismaService.toyListing.findFirstOrThrow({
			where: {
				images: {
					some: {
						id: imageId,
					},
				},
			},
		});
	}

	async likeListing(userId: number, listingId: number, like: boolean = true) {
		return this.prismaService.userLike.upsert({
			create: {
				userId,
				listingId,
				liked: like,
			},
			update: {
				liked: like,
			},
			where: {
				userId_listingId: {
					userId,
					listingId,
				},
			},
		});
	}
}
