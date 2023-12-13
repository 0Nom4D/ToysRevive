import { ApiProperty } from "@nestjs/swagger";
import { IsMobilePhone, IsNotEmpty, IsString } from "class-validator";
import { IntersectionType, PartialType } from "@nestjs/mapped-types";
import { CreateUser } from "src/prisma/models";

export class CreateUserDTO extends IntersectionType(CreateUser) {
	@ApiProperty({
		description: 'the phone number of the user',
	})
	@IsNotEmpty()
	@IsString()
	@IsMobilePhone()
	phone!: string;

	@ApiProperty({
		description: "the hashed password of the user's account",
	})
	@IsNotEmpty()
	@IsString()
	email!: string;
}

export class UpdateUserDTO extends PartialType(CreateUserDTO) {}