import { ApiProperty, PickType } from '@nestjs/swagger';
import { ListingImage, ToyListing } from 'src/prisma/models';

class ImageResponse extends PickType(ListingImage, ['id', 'blurhash']) {}

export class ToyListingResponse extends ToyListing {
	@ApiProperty({
		isArray: true,
		type: ImageResponse,
		description: "The listing's images",
	})
	images: ImageResponse[];
}
