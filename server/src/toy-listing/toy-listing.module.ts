import { Module, forwardRef } from '@nestjs/common';
import { ToyListingService } from './toy-listing.service';
import { ToyListingController } from './toy-listing.controller';
import { PrismaModule } from 'src/prisma/prisma.module';
import { ImagesModule } from 'src/image/image.module';

@Module({
	imports: [PrismaModule, forwardRef(() => ImagesModule)],
	providers: [ToyListingService],
	controllers: [ToyListingController],
	exports: [ToyListingService],
})
export class ToyListingModule {}
