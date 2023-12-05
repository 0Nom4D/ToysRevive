package nom4d.toysrevive.swipe

import android.os.Build
import android.view.HapticFeedbackConstants
import androidx.annotation.RequiresApi

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding

import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.SheetState

import androidx.compose.runtime.Composable
import androidx.compose.runtime.rememberCoroutineScope

import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalView
import androidx.compose.ui.unit.dp

import coil.compose.rememberAsyncImagePainter

import com.alexstyl.swipeablecard.Direction
import com.alexstyl.swipeablecard.ExperimentalSwipeableCardApi
import com.alexstyl.swipeablecard.rememberSwipeableCardState
import com.alexstyl.swipeablecard.swipableCard

import kotlinx.coroutines.launch

import nom4d.toysrevive.settings.AppSettings

@RequiresApi(Build.VERSION_CODES.R)
@OptIn(ExperimentalSwipeableCardApi::class, ExperimentalMaterial3Api::class)
@Composable
fun SwipeView(bottomSheetState: SheetState, modifier: Modifier = Modifier) {
    val coroutineScope = rememberCoroutineScope()
    val state = rememberSwipeableCardState()
    val view = LocalView.current

    Column(horizontalAlignment = Alignment.CenterHorizontally, verticalArrangement = Arrangement.Center, modifier = modifier.fillMaxSize()) {
        Box(
            contentAlignment = Alignment.Center,
            modifier = Modifier
                .fillMaxSize()
                .padding(15.dp)
        ) {
//                Column(horizontalAlignment = Alignment.CenterHorizontally, verticalArrangement = Arrangement.Center, modifier = Modifier.fillMaxSize()) {
//                    Icon(
//                        painterResource(id = R.drawable.baseline_no_likes_24),
//                        contentDescription = null,
//                        modifier = Modifier.fillMaxSize(.6f)
//                    )
//                    Text(
//                        "There is no toy left to see there. Check later to see if new people wants to make this world better!",
//                        textAlign = TextAlign.Center,
//                        fontFamily = getPoppins(),
//                        fontWeight = FontWeight.Medium,
//                        fontSize = 12.sp,
//                    )
//                }
            SwipeableImageCard(
                painter = rememberAsyncImagePainter("https://pbs.twimg.com/media/Ex0MTkpXMAIE87u.jpg"),
                contentDescription = "Test for home page",
                imageTitle = "KFC Espagne",
                state = state,
                modifier = Modifier.swipableCard(
                    state = state,
                    blockedDirections = listOf(Direction.Up, Direction.Down),
                    onSwiped = { direction ->
                        when (direction) {
                            Direction.Right -> {
                                coroutineScope.launch {
                                    state.swipe(Direction.Right)
                                }
                                view.performHapticFeedback(HapticFeedbackConstants.CONFIRM)
                            }
                            Direction.Left -> {
                                coroutineScope.launch {
                                    state.swipe(Direction.Left)
                                }
                                view.performHapticFeedback(HapticFeedbackConstants.REJECT)

                            }
                            else -> {}
                        }
                    },
                ),
                onDetailButtonClicked = {
                    AppSettings.isToyDescription.value = true
                    coroutineScope.launch {
                        bottomSheetState.show()
                    }
                }
            )
        }
    }
}