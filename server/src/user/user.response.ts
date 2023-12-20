import { OmitType } from '@nestjs/swagger';
import { Exclude } from 'class-transformer';
import { User } from 'src/prisma/models';

abstract class BaseUserResponse extends OmitType(User, ['password']) {
	@Exclude({ toPlainOnly: true })
	password: string;
}

export class AuthedUserResponse extends BaseUserResponse {
	constructor(partial: AuthedUserResponse) {
		super();
		Object.assign(this, partial);
	}
}

export class PublicUserResponse extends OmitType(AuthedUserResponse, [
	'email',
	'lastName',
	'firstName',
	'phone',
]) {
	@Exclude({ toPlainOnly: true })
	email: string;
	@Exclude({ toPlainOnly: true })
	lastName: string;
	@Exclude({ toPlainOnly: true })
	firstName: string;
	@Exclude({ toPlainOnly: true })
	phone: string;

	constructor(partial: PublicUserResponse) {
		super();
		Object.assign(this, partial);
	}
}
