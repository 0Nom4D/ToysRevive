package nom4d.toysrevive.auth.steps

import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.pager.PagerState
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.ArrowBack
import androidx.compose.material.icons.filled.Check
import androidx.compose.material.icons.filled.Visibility
import androidx.compose.material.icons.filled.VisibilityOff
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
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.input.PasswordVisualTransformation
import androidx.compose.ui.text.input.VisualTransformation
import androidx.hilt.navigation.compose.hiltViewModel
import androidx.navigation.NavHostController
import kotlinx.coroutines.launch
import nom4d.toysrevive.api.dtos.RegisterDto
import nom4d.toysrevive.auth.RegisterViewModel
import nom4d.toysrevive.navigation.NavigationItem
import nom4d.toysrevive.validators.Validators

@OptIn(ExperimentalFoundationApi::class)
@Composable
fun PasswordStep(
    dto: RegisterDto,
    pagerState: PagerState,
    navHostController: NavHostController,
    modifier: Modifier = Modifier,
    viewModel: RegisterViewModel = hiltViewModel()
) {
    var password by remember { mutableStateOf("") }
    var passwordVisibility by remember { mutableStateOf(false) }

    var confirmPassword by remember { mutableStateOf("") }
    var confirmPasswordError by remember { mutableStateOf("") }
    var confirmPasswordVisibility by remember { mutableStateOf(false) }

    var isPasswordInvalid by remember { mutableStateOf(false) }

    val coroutineScope = rememberCoroutineScope()

    Column(verticalArrangement = Arrangement.Center, modifier = Modifier.fillMaxSize()) {
        Box(
            modifier = modifier
                .align(Alignment.CenterHorizontally)
                .fillMaxHeight(.3f)
                .fillMaxWidth(.9f)
        ) {
            Column(
                verticalArrangement = Arrangement.SpaceEvenly,
                horizontalAlignment = Alignment.CenterHorizontally,
                modifier = Modifier.align(Alignment.Center).fillMaxHeight()
            ) {
                OutlinedTextField(
                    value = password,
                    onValueChange = { password = it },
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
                    modifier = Modifier.fillMaxWidth(.9f)
                )
                OutlinedTextField(
                    value = confirmPassword,
                    onValueChange = { confirmPassword = it },
                    trailingIcon = {
                        IconButton(onClick = {
                            confirmPasswordVisibility = !confirmPasswordVisibility
                        }) {
                            if (!confirmPasswordVisibility) {
                                Icon(Icons.Filled.Visibility, contentDescription = "")
                            } else {
                                Icon(Icons.Filled.VisibilityOff, contentDescription = "")
                            }
                        }
                    },
                    singleLine = true,
                    visualTransformation = if (!confirmPasswordVisibility) {
                        PasswordVisualTransformation()
                    } else {
                        VisualTransformation.Companion.None
                    },
                    isError = isPasswordInvalid,
                    supportingText = {
                        if (isPasswordInvalid) {
                            Text(
                                modifier = Modifier.fillMaxWidth(),
                                text = confirmPasswordError,
                                color = MaterialTheme.colorScheme.error
                            )
                        }
                    },
                    modifier = Modifier.fillMaxWidth(.9f)
                )
                Row(
                    verticalAlignment = Alignment.CenterVertically,
                    horizontalArrangement = Arrangement.SpaceBetween,
                    modifier = Modifier.fillMaxWidth()
                ) {
                    TextButton(
                        onClick = {
                            coroutineScope.launch {
                                pagerState.animateScrollToPage(pagerState.currentPage - 1)
                            }
                        }
                    ) {
                        Icon(Icons.Default.ArrowBack, contentDescription = "Précédent")
                        Text("Précédent")
                    }
                    TextButton(
                        onClick = {
                            isPasswordInvalid = false
                            if (password != confirmPassword) {
                                confirmPasswordError = "Passwords don't match."
                                isPasswordInvalid = true
                                return@TextButton
                            }
                            dto.password = password
                            val validator = Validators.validate(dto)
                            val errorList = validator.second[RegisterDto::password]
                            if (!errorList.isNullOrEmpty()) {
                                password = "Password " +
                                    "${errorList.firstOrNull() ?: "contains an unknown error"}."
                                isPasswordInvalid = true
                            }
                            if (!validator.first || isPasswordInvalid) {
                                return@TextButton
                            }
                            if (viewModel.registerUser(dto)) {
                                navHostController.navigate(NavigationItem.LoginScreen.routeName)
                            } else {
                                confirmPasswordError = "Une erreur interne est survenue."
                                isPasswordInvalid = true
                            }
                        }
                    ) {
                        Text("Terminer")
                        Icon(Icons.Default.Check, contentDescription = "Terminer")
                    }
                }
            }
        }
    }
}
