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
import androidx.compose.ui.text.input.KeyboardType
import kotlinx.coroutines.launch
import nom4d.toysrevive.api.dtos.RegisterDto
import nom4d.toysrevive.validators.Validators

@OptIn(ExperimentalFoundationApi::class)
@Composable
fun PhoneStep(
    dto: RegisterDto,
    pagerState: PagerState,
    modifier: Modifier = Modifier
) {
    var phone by remember { mutableStateOf("") }
    var phoneError by remember { mutableStateOf("") }
    var isPhoneInvalid by remember { mutableStateOf(false) }

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
                    value = phone,
                    onValueChange = {
                        phone = it
                    },
                    placeholder = {
                        Text("Numéro de téléphone")
                    },
                    singleLine = true,
                    keyboardOptions = KeyboardOptions(keyboardType = KeyboardType.Phone),
                    isError = isPhoneInvalid,
                    supportingText = {
                        if (isPhoneInvalid) {
                            Text(
                                modifier = Modifier.fillMaxWidth(),
                                text = phoneError,
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
                            isPhoneInvalid = false
                            dto.phone = phone
                            val validator = Validators.validate(dto)

                            val errorList = validator.second[RegisterDto::phone]
                            if (!errorList.isNullOrEmpty()) {
                                phoneError = "Phone " +
                                        "${errorList.firstOrNull() ?: "contains an unknown error"}."
                                isPhoneInvalid = true
                            }
                            if (isPhoneInvalid) {
                                return@TextButton
                            }
                            coroutineScope.launch {
                                pagerState.animateScrollToPage(pagerState.currentPage + 1)
                            }
                        }
                    ) {
                        Text("Suivant")
                        Icon(Icons.Default.ArrowForward, contentDescription = "Suivant")
                    }
                }
            }
        }
    }
}
