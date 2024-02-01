import { HttpException, HttpStatus, Injectable } from '@nestjs/common';
import LoginDTO from 'src/authentication/models/login.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import * as bcrypt from 'bcrypt';
import { CreateUserDTO } from './user.dto';
import { PaginationParameters } from 'src/pagination/models/pagination-parameters';

@Injectable()
export class UserService {
	constructor(private prismaService: PrismaService) {}

	private readonly passwordHashSaltRound = 9;

	private encryptPassword(plainTextPassword: string): string {
		return bcrypt.hashSync(plainTextPassword, this.passwordHashSaltRound);
	}

	async getByCredentials(loginDto: LoginDTO) {
		const user = await this.prismaService.user.findFirstOrThrow({
			where: { userName: loginDto.username },
		});

		if (!bcrypt.compareSync(loginDto.password, user.password)) {
			throw new HttpException(
				'Username or password is incorrect',
				HttpStatus.NOT_FOUND,
			);
		}
		return user;
	}

	async getById(id: number) {
		return this.prismaService.user.findFirstOrThrow({ where: { id: id } });
	}

	async getMany(pagination: PaginationParameters) {
		return this.prismaService.user.findMany({
			take: pagination.take,
			skip: pagination.skip,
		});
	}

	createUser(registrationDTO: CreateUserDTO) {
		return this.prismaService.user.create({
			data: {
				phone: registrationDTO.phone,
				userName: registrationDTO.userName,
				email: registrationDTO.email,
				firstName: registrationDTO.firstName,
				lastName: registrationDTO.lastName,
				password: this.encryptPassword(registrationDTO.password),
			},
		});
	}
}
