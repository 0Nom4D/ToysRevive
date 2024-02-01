import {
	Controller,
	Delete,
	Get,
	HttpException,
	HttpStatus,
	Param,
	ParseFilePipeBuilder,
	ParseIntPipe,
	Post,
	Request,
	Response,
	UploadedFile,
	UseGuards,
	UseInterceptors,
} from '@nestjs/common';
import { ImageService } from './image.service';
import { ApiBearerAuth, ApiOperation, ApiTags } from '@nestjs/swagger';
import { ToyListingService } from 'src/toy-listing/toy-listing.service';
import JwtAuthGuard from 'src/authentication/jwt/jwt-auth.guard';
import { FileInterceptor } from '@nestjs/platform-express';

@ApiTags('Images')
@Controller('images')
export class ImageController {
	constructor(
		protected imageService: ImageService,
		protected toyListingService: ToyListingService,
	) {}

	@Get(':id')
	@ApiBearerAuth()
	@ApiOperation({
		summary: 'Get A Single Image',
	})
	@UseGuards(JwtAuthGuard)
	public getListingImage(
		@Param('id', ParseIntPipe) id: number,
		@Response({ passthrough: true }) res: any,
	) {
		return this.imageService.streamImage(id, res);
	}

	@Post(':listingId')
	@ApiBearerAuth()
	@ApiOperation({
		summary: 'Create An Image for a listing',
	})
	@UseInterceptors(FileInterceptor('file'))
	@UseGuards(JwtAuthGuard)
	public async createListingImage(
		@Param('listingId', ParseIntPipe) id: number,
		@Request() req: any,
		@UploadedFile(
			new ParseFilePipeBuilder()
				.addFileTypeValidator({
					fileType: /(png|jpeg|jpg)$/i,
				})
				.build({
					errorHttpStatusCode: HttpStatus.UNPROCESSABLE_ENTITY,
				}),
		)
		file: Express.Multer.File,
	) {
		const parentListing = await this.toyListingService.get(id);
		const authedUserId = req.user.id;

		if (parentListing.ownerId !== authedUserId) {
			throw new HttpException(
				'You can not create an image for a listing that is not yours.',
				HttpStatus.UNAUTHORIZED,
			);
		}
		return this.imageService.createImage(parentListing.id, file.buffer);
	}

	@Delete(':id')
	@ApiBearerAuth()
	@ApiOperation({
		summary: 'Delete A Single Image',
	})
	@UseGuards(JwtAuthGuard)
	public async deleteListingImage(
		@Param('id', ParseIntPipe) id: number,
		@Request() req: any,
	) {
		const parentListing = await this.toyListingService.getParentListing(id);
		const authedUserId = req.user.id;

		if (parentListing.ownerId !== authedUserId) {
			throw new HttpException(
				'You can not delete an image that is not yours.',
				HttpStatus.UNAUTHORIZED,
			);
		}
		return this.imageService.deleteImage(id);
	}
}
