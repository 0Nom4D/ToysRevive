import { ApiProperty } from '@nestjs/swagger';
import {
	IsEmail,
	IsMobilePhone,
	IsNotEmpty,
	IsString,
	MinLength,
} from 'class-validator';
import { PartialType } from '@nestjs/mapped-types';
import { CreateUser } from 'src/prisma/models';

export class CreateUserDTO extends CreateUser {
	@MinLength(6)
	userName: string;
	@MinLength(7)
	password: string;
	@ApiProperty({
		description: 'The phone number of the User',
		example: '0123456789',
	})
	@IsNotEmpty()
	@IsString()
	@IsMobilePhone()
	phone!: string;

	@ApiProperty({
		description: 'The Email of the User',
	})
	@IsNotEmpty()
	@IsEmail()
	@IsString()
	email!: string;
}

export class UpdateUserDTO extends PartialType(CreateUserDTO) {}
