package nom4d.toysrevive.validators

import io.konform.validation.Validation
import io.konform.validation.ValidationResult
import io.konform.validation.jsonschema.maximum
import io.konform.validation.jsonschema.minLength
import io.konform.validation.jsonschema.minimum
import io.konform.validation.jsonschema.pattern
import nom4d.toysrevive.api.dtos.CreateListingDto
import nom4d.toysrevive.api.dtos.LoginDto
import nom4d.toysrevive.api.dtos.RegisterDto

class Validators {
    companion object {
        fun validate(loginDto: LoginDto): Pair<Boolean, ValidationResult<LoginDto>> {
            val validator = Validation {
                LoginDto::username {
                    minLength(6)
                }
                LoginDto::password {
                    minLength(7)
                }
            }
            val ret = validator(loginDto)
            return Pair(ret.errors.isEmpty(), ret)
        }

        fun validate(registerDto: RegisterDto): Pair<Boolean, ValidationResult<RegisterDto>> {
            val validator = Validation {
                RegisterDto::userName {
                    minLength(6)
                }
                RegisterDto::email {
                    pattern(".+@.+\\..+")
                }
                RegisterDto::password {
                    minLength(7)
                }
                RegisterDto::phone {
                    pattern("^(?:(?:\\+|00)33|0)\\s*[1-9](?:[\\s.-]*\\d{2}){4}\$")
                }
            }
            val ret = validator(registerDto)
            return Pair(ret.errors.isEmpty(), ret)
        }

        fun validate(
            createListingDto: CreateListingDto
        ): Pair<Boolean, ValidationResult<CreateListingDto>> {
            val validator = Validation {
                CreateListingDto::postCode {
                    minimum(1000)
                    maximum(99999)
                }
            }
            val ret = validator(createListingDto)
            return Pair(ret.errors.isEmpty(), ret)
        }
    }
}
