import { OmitType } from '@nestjs/swagger';
import { Exclude } from 'class-transformer';
import { User } from 'src/prisma/models';

export class UserResponse extends OmitType(User, ['password']) {
	@Exclude({ toPlainOnly: true })
	password: string;

	constructor(partial: UserResponse) {
		super();
		Object.assign(this, partial);
	}
}
