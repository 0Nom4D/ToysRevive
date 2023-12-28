-- CreateEnum
CREATE TYPE "condition" AS ENUM ('New', 'VeryGood', 'Good', 'Acceptable', 'Bad');

-- CreateEnum
CREATE TYPE "toy_type" AS ENUM ('Plastic', 'Wood', 'Dinosaur', 'Car', 'VideoGame', 'ConstructionToy', 'BoardGame', 'Other');

-- CreateTable
CREATE TABLE "toy_offers" (
    "id" SERIAL NOT NULL,
    "title" TEXT NOT NULL,
    "condition" "condition" NOT NULL,
    "type" "toy_type" NOT NULL,
    "ownerId" INTEGER NOT NULL,
    "postCode" INTEGER NOT NULL,
    "addDate" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,

    CONSTRAINT "toy_offers_pkey" PRIMARY KEY ("id")
);

-- AddForeignKey
ALTER TABLE "toy_offers" ADD CONSTRAINT "toy_offers_ownerId_fkey" FOREIGN KEY ("ownerId") REFERENCES "users"("id") ON DELETE CASCADE ON UPDATE CASCADE;
