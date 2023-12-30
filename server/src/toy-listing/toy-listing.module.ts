import { Module } from '@nestjs/common';
import { ToyListingService } from './toy-listing.service';
import { ToyListingController } from './toy-listing.controller';
import { PrismaModule } from 'src/prisma/prisma.module';

@Module({
	imports: [PrismaModule],
	providers: [ToyListingService],
	controllers: [ToyListingController],
	exports: [ToyListingService],
})
export class ToyListingModule {}
