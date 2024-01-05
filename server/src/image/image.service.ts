import {
	HttpException,
	HttpStatus,
	Injectable,
	StreamableFile,
} from '@nestjs/common';
import { ListingImage } from 'src/prisma/models';
import { PrismaService } from 'src/prisma/prisma.service';
import * as Jimp from 'jimp';
import * as Blurhash from 'blurhash';
import { existsSync, mkdirSync, rm } from 'fs';
import { basename } from 'path';
import * as mime from 'mime';
import { Readable } from 'stream';

@Injectable()
export class ImageService {
	private readonly imagesDir = './data/listings/';
	constructor(protected prismaService: PrismaService) {
		if (!existsSync(this.imagesDir)) {
			mkdirSync(this.imagesDir, { recursive: true });
		}
	}

	async createImage(
		listingId: number,
		imageBuffer: Buffer,
	): Promise<ListingImage> {
		const image = await Jimp.read(imageBuffer);
		const blurhash = await new Promise<string>((resolve) => {
			const array = new Uint8ClampedArray(image.bitmap.data.length);

			image.bitmap.data.map((char, index) => (array[index] = char));
			resolve(
				Blurhash.encode(
					array,
					image.getWidth(),
					image.getHeight(),
					// Represent the max number of colors on each axis
					4,
					4,
				),
			);
		});

		const listingImage = await this.prismaService.listingImage.create({
			data: {
				blurhash: blurhash,
				listingId: listingId,
			},
		});

		image.writeAsync(`${this.imagesDir}/${listingImage.id}.jpg`);
		return listingImage;
	}

	async streamImage(imageId: number, res: any) {
		const image = await this.prismaService.listingImage.findFirstOrThrow({
			where: { id: imageId },
		});
		const imagePath = `${this.imagesDir}/${image.id}.jpg`;

		if (!existsSync(imagePath)) {
			throw new HttpException(
				'Image file not found',
				HttpStatus.NOT_FOUND,
			);
		}

		res.set({
			'Content-Disposition': `attachment; filename="${basename(
				imagePath,
			)}"`,
		});

		return Jimp.read(imagePath)
			.then((jimpImage) =>
				jimpImage.getBufferAsync(mime.getType(imagePath)),
			)
			.then((buffer) => new StreamableFile(Readable.from(buffer)))
			.catch(() => {
				throw new HttpException(
					'An error occured while processing the image',
					HttpStatus.INTERNAL_SERVER_ERROR,
				);
			});
	}

	async deleteImage(imageId: number): Promise<ListingImage> {
		return this.prismaService.listingImage
			.delete({
				where: { id: imageId },
			})
			.then((deletedImage) => {
				rm(`${this.imagesDir}/${deletedImage.id}.jpg`, () => {});
				return deletedImage;
			});
	}
}
