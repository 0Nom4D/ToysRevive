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
import androidx.compose.material.icons.filled.ArrowForward
import androidx.compose.material3.Icon
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
import androidx.compose.ui.unit.sp
import androidx.navigation.NavHostController
import kotlinx.coroutines.launch
import nom4d.toysrevive.api.dtos.RegisterDto
import nom4d.toysrevive.navigation.NavigationItem

@OptIn(ExperimentalFoundationApi::class)
@Composable
fun NameStep(
    navHostController: NavHostController,
    dto: RegisterDto,
    pagerState: PagerState,
    modifier: Modifier = Modifier
) {
    // Names
    var lastName by remember { mutableStateOf("") }
    var firstName by remember { mutableStateOf("") }

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
                    value = firstName,
                    onValueChange = {
                        firstName = it
                    },
                    placeholder = {
                        Text("Prénom")
                    },
                    singleLine = true,
                    modifier = Modifier.fillMaxWidth(.9f)
                )
                OutlinedTextField(
                    value = lastName,
                    onValueChange = {
                        lastName = it
                    },
                    placeholder = {
                        Text("Nom")
                    },
                    singleLine = true,
                    modifier = Modifier.fillMaxWidth(.9f)
                )
                Row(
                    verticalAlignment = Alignment.CenterVertically,
                    horizontalArrangement = Arrangement.SpaceBetween,
                    modifier = Modifier.fillMaxWidth(.9f)
                ) {
                    TextButton(onClick = {
                        navHostController.navigate(NavigationItem.LoginScreen.routeName)
                    }) {
                        Text("Déjà un compte?", fontSize = 12.sp)
                    }
                    TextButton(
                        onClick = {
                            dto.firstName = firstName
                            dto.lastName = lastName
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
