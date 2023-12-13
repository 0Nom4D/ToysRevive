import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { PrismaModule } from './prisma/prisma.module';
import { UserModule } from './user/user.module';
import { AppProviders } from './app.plugins';

@Module({
	imports: [PrismaModule, UserModule],
	controllers: [AppController],
	providers: AppProviders,
})
export class AppModule {}
