import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import * as Plugins from './app.plugins';
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger';

async function bootstrap() {
	Plugins.presetup();
	const app = await NestFactory.create(AppModule);

	app.useGlobalFilters(...Plugins.buildExceptionFilters(app));
	app.useGlobalPipes(...Plugins.buildPipes(app));
	app.useGlobalInterceptors(...Plugins.buildInterceptors(app));
	app.use(...Plugins.buildHttpPlugs(app));

	const config = new DocumentBuilder()
		.setTitle('ToysRevive Server')
		.setDescription('API description')
		.setVersion('1.0')
		.build();
	const document = SwaggerModule.createDocument(app, config);

	SwaggerModule.setup('/swagger', app, document, {
		customSiteTitle: 'Swagger - ToysRevive',
		swaggerOptions: {
			tagsSorter: 'alpha',
			operationsSorter: 'alpha',
		},
	});

	await app.listen(3000);
}

bootstrap();
