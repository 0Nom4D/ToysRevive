-- CreateTable
CREATE TABLE "UserLike" (
    "id" SERIAL NOT NULL,
    "userId" INTEGER NOT NULL,
    "listingId" INTEGER NOT NULL,
    "liked" BOOLEAN NOT NULL,

    CONSTRAINT "UserLike_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE UNIQUE INDEX "UserLike_userId_listingId_key" ON "UserLike"("userId", "listingId");

-- AddForeignKey
ALTER TABLE "UserLike" ADD CONSTRAINT "UserLike_userId_fkey" FOREIGN KEY ("userId") REFERENCES "users"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "UserLike" ADD CONSTRAINT "UserLike_listingId_fkey" FOREIGN KEY ("listingId") REFERENCES "toy_offers"("id") ON DELETE CASCADE ON UPDATE CASCADE;
