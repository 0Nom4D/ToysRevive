package nom4d.toysrevive.auth

import androidx.compose.foundation.border
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Visibility
import androidx.compose.material.icons.filled.VisibilityOff
import androidx.compose.material3.Button
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.input.PasswordVisualTransformation
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.hilt.navigation.compose.hiltViewModel
import androidx.navigation.NavHostController
import nom4d.toysrevive.api.dtos.LoginDto
import nom4d.toysrevive.navigation.NavigationItem
import nom4d.toysrevive.validators.Validators

@Composable
fun LoginView(
    navHostController: NavHostController,
    modifier: Modifier = Modifier,
    viewModel: LoginViewModel = hiltViewModel()
) {
    var username by remember { mutableStateOf("") }
    var isUsernameInvalid by remember { mutableStateOf(false) }

    // Password
    var password by remember { mutableStateOf("") }
    var passwordVisibility by remember { mutableStateOf(false) }
    var isPasswordInvalid by remember { mutableStateOf(false) }

    // Errors
    var usernameError by remember { mutableStateOf("") }
    var passwordError by remember { mutableStateOf("") }

    Column(
        verticalArrangement = Arrangement.Center,
        horizontalAlignment = Alignment.CenterHorizontally,
        modifier = modifier
            .fillMaxSize()
    ) {
        OutlinedTextField(
            value = username,
            onValueChange = {
                username = it
            },
            placeholder = {
                Text("Nom d'utilisateur")
            },
            singleLine = true,
            isError = isUsernameInvalid,
            supportingText = {
                if (isUsernameInvalid) {
                    Text(
                        modifier = Modifier.fillMaxWidth(),
                        text = usernameError,
                        color = MaterialTheme.colorScheme.error
                    )
                }
            },
            modifier = Modifier.fillMaxWidth(.9f)
        )
        OutlinedTextField(
            value = password,
            onValueChange = {
                password = it
            },
            placeholder = {
                Text("Mot de passe")
            },
            trailingIcon = {
                IconButton(onClick = { passwordVisibility = !passwordVisibility }) {
                    if (!passwordVisibility) {
                        Icon(Icons.Filled.Visibility, contentDescription = "")
                    } else {
                        Icon(Icons.Filled.VisibilityOff, contentDescription = "")
                    }
                }
            },
            singleLine = true,
            visualTransformation = if (!passwordVisibility) {
                PasswordVisualTransformation()
            } else {
                VisualTransformation.Companion.None
            },
            isError = isPasswordInvalid,
            supportingText = {
                if (isPasswordInvalid) {
                    Text(
                        modifier = Modifier.fillMaxWidth(),
                        text = passwordError,
                        color = MaterialTheme.colorScheme.error
                    )
                }
            },
            modifier = Modifier.fillMaxWidth(.9f)
        )
        Row(
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.End,
            modifier = Modifier.fillMaxWidth(.9f).border(5.dp, Color.Red)
        ) {
            Text("Pas encore de compte? ", fontSize = 12.sp)
            TextButton(onClick = {
                navHostController.navigate(NavigationItem.RegisterScreen.routeName)
            }) {
                Text("Se créer un compte", fontSize = 12.sp)
            }
        }
        Button(onClick = {
            val loginDto = LoginDto(username, password)
            val validator = Validators.validate(loginDto)
            if (validator.first) {
                viewModel.loginUser(loginDto)
                navHostController.navigate("home")
            } else {
                val validation = validator.second
                if (validation[LoginDto::username] != null) {
                    usernameError = "Username " +
                        "${validation[LoginDto::username]?.first() ?: "contains an unknown error"}."
                    isUsernameInvalid = true
                } else if (validation[LoginDto::password] != null) {
                    passwordError = "Password " +
                        "${validation[LoginDto::password]?.first() ?: "contains an unknown error"}."
                    isPasswordInvalid = true
                }
            }
        }) {
            Text("Se connecter")
        }
    }
}
