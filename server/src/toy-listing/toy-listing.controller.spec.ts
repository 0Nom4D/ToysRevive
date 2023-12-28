import { Test, TestingModule } from '@nestjs/testing';
import { ToyListingController } from './toy-listing.controller';

describe('ToyListingController', () => {
  let controller: ToyListingController;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      controllers: [ToyListingController],
    }).compile();

    controller = module.get<ToyListingController>(ToyListingController);
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
  });
});
