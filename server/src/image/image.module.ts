import { Module, forwardRef } from '@nestjs/common';
import { ImageService } from './image.service';
import { ImageController } from './image.controller';
import { ToyListingModule } from 'src/toy-listing/toy-listing.module';
import { PrismaModule } from 'src/prisma/prisma.module';

@Module({
	imports: [forwardRef(() => ToyListingModule), PrismaModule],
	exports: [ImageService],
	providers: [ImageService],
	controllers: [ImageController],
})
export class ImagesModule {}
