import { MiddlewareConsumer, Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { PrismaModule } from './prisma/prisma.module';
import { UserModule } from './user/user.module';
import { AppProviders, applyMiddlewares } from './app.plugins';
import { AuthenticationModule } from './authentication/authentication.module';

@Module({
	imports: [
		PrismaModule,
		UserModule,
		AuthenticationModule
	],
	controllers: [AppController],
	providers: AppProviders,
})
export class AppModule {
	configure(consumer: MiddlewareConsumer) {
		applyMiddlewares(consumer);
	}
}
