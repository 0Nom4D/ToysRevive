import { ApiProperty } from "@nestjs/swagger";
import { IsEmail, IsMobilePhone, IsNotEmpty, IsString, MinLength } from "class-validator";
import { IntersectionType, PartialType } from "@nestjs/mapped-types";
import { CreateUser } from "src/prisma/models";

export class CreateUserDTO extends IntersectionType(CreateUser) {
	@MinLength(6)
	userName: string;
	@MinLength(7)
	password: string;
	@ApiProperty({
		description: 'the phone number of the user',
		example: '0123456789'
	})
	@IsNotEmpty()
	@IsString()
	@IsMobilePhone()
	phone!: string;

	@ApiProperty({
		description: "the hashed password of the user's account",
	})
	@IsNotEmpty()
	@IsEmail()
	@IsString()
	email!: string;
}

export class UpdateUserDTO extends PartialType(CreateUserDTO) {}