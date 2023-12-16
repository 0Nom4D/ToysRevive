import { Injectable } from '@nestjs/common';
import LoginDTO from 'src/authentication/models/login.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import * as bcrypt from 'bcrypt';

@Injectable()
export class UserService {
	constructor(private prismaService: PrismaService) {}

	private readonly passwordHashSaltRound = 9;

	private encryptPassword(plainTextPassword: string): string {
		return bcrypt.hashSync(plainTextPassword, this.passwordHashSaltRound);
	}

	getByCredentials(loginDto: LoginDTO) {
		return this.prismaService.user.findFirstOrThrow({
			where: {
				userName: loginDto.username,
				password: this.encryptPassword(loginDto.password)
			}
		})
	}
}
