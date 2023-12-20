import { Injectable, Logger } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';

@Injectable()
export default class TestPrismaService extends PrismaService {
	private readonly logger = new Logger(PrismaService.name);
	override async onModuleInit() {
		await this.$connect();
		await this.flushDatabase();
	}

	protected async flushDatabase() {
		this.logger.warn('Flushing database');
		const tablenames = await this.$queryRaw<
			Array<{ tablename: string }>
		>`SELECT tablename FROM pg_tables WHERE schemaname='public'`;

		for (const { tablename } of tablenames) {
			if (tablename !== '_prisma_migrations') {
				try {
					await this.$executeRawUnsafe(
						`TRUNCATE TABLE "public"."${tablename}" CASCADE;`,
					);
				} catch (error) {
					this.logger.error(`Flushing table '${tablename}' failed`);
				}
			}
		}
	}
}
