import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import * as Plugins from './app.plugins';

async function bootstrap() {
	Plugins.presetup();
	const app = await NestFactory.create(AppModule);

	app.useGlobalFilters(...Plugins.buildExceptionFilters(app))
		.useGlobalPipes(...Plugins.buildPipes(app))
		.useGlobalInterceptors(...Plugins.buildInterceptors(app))
		.use(...Plugins.buildHttpPlugs(app));
	await app.listen(3000);
}

bootstrap();
