-- CreateTable
CREATE TABLE "images" (
    "id" SERIAL NOT NULL,
    "listingId" INTEGER NOT NULL,

    CONSTRAINT "images_pkey" PRIMARY KEY ("id")
);

-- AddForeignKey
ALTER TABLE "images" ADD CONSTRAINT "images_listingId_fkey" FOREIGN KEY ("listingId") REFERENCES "toy_offers"("id") ON DELETE CASCADE ON UPDATE CASCADE;
