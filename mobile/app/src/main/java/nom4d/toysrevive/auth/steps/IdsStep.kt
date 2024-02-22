package nom4d.toysrevive.auth.steps

import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.border
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.pager.PagerState
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.ArrowBack
import androidx.compose.material.icons.filled.ArrowForward
import androidx.compose.material3.Icon
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
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.input.KeyboardType
import androidx.compose.ui.unit.dp
import kotlinx.coroutines.launch
import nom4d.toysrevive.api.dtos.RegisterDto
import nom4d.toysrevive.validators.Validators

@OptIn(ExperimentalFoundationApi::class)
@Composable
fun IdsStep(dto: RegisterDto, pagerState: PagerState, modifier: Modifier = Modifier) {
    // Email
    var email by remember { mutableStateOf("") }
    var isEmailInvalid by remember { mutableStateOf(false) }

    // Username
    var username by remember { mutableStateOf("") }
    var isUsernameInvalid by remember { mutableStateOf(false) }

    // Errors
    var emailError by remember { mutableStateOf("") }
    var usernameError by remember { mutableStateOf("") }

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
                modifier = Modifier
                    .align(Alignment.Center)
                    .fillMaxHeight()
            ) {
                OutlinedTextField(
                    value = email,
                    onValueChange = {
                        email = it
                    },
                    placeholder = {
                        Text("Adresse mail")
                    },
                    singleLine = true,
                    keyboardOptions = KeyboardOptions(keyboardType = KeyboardType.Email),
                    isError = isEmailInvalid,
                    supportingText = {
                        if (isEmailInvalid) {
                            Text(
                                modifier = Modifier.fillMaxWidth(),
                                text = emailError,
                                color = MaterialTheme.colorScheme.error
                            )
                        }
                    },
                    modifier = Modifier.fillMaxWidth(.9f)
                )
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
                        },
                        modifier = Modifier.border(5.dp, Color.Green)
                    ) {
                        Icon(Icons.Default.ArrowBack, contentDescription = "Précédent")
                        Text("Précédent")
                    }
                    TextButton(
                        onClick = {
                            isEmailInvalid = false
                            isUsernameInvalid = false
                            dto.email = email
                            dto.userName = username
                            val validator = Validators.validate(dto)

                            for (prop in listOf(RegisterDto::email, RegisterDto::userName)) {
                                val error = validator.second[prop]

                                if (error.isNullOrEmpty()) {
                                    continue
                                }
                                when (prop) {
                                    RegisterDto::userName -> {
                                        usernameError = "Username " +
                                            "${error.firstOrNull() ?: "contains an unknown error"}."
                                        isUsernameInvalid = true
                                    }
                                    RegisterDto::email -> {
                                        emailError = "Email " +
                                            "${error.firstOrNull() ?: "contains an unknown error"}."
                                        isEmailInvalid = true
                                    }
                                }
                            }
                            if (isUsernameInvalid || isEmailInvalid) {
                                return@TextButton
                            }
                            coroutineScope.launch {
                                pagerState.animateScrollToPage(pagerState.currentPage + 1)
                            }
                        },
                        modifier = Modifier.border(5.dp, Color.Blue)
                    ) {
                        Text("Suivant")
                        Icon(Icons.Default.ArrowForward, contentDescription = "Suivant")
                    }
                }
            }
        }
    }
}
