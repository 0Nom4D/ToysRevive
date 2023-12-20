import { INestApplication } from "@nestjs/common";
import type { TestingModule } from "@nestjs/testing";
import { buildExceptionFilters, buildPipes, buildInterceptors, buildHttpPlugs } from '../src/app.plugins';

export default async function SetupApp(module: TestingModule): Promise<INestApplication> {
	const app = module.createNestApplication();

	app.useGlobalFilters(...buildExceptionFilters(app))
	app.useGlobalPipes(...buildPipes(app))
	app.useGlobalInterceptors(...buildInterceptors(app))
	app.use(...buildHttpPlugs(app));
	return app.init();
}