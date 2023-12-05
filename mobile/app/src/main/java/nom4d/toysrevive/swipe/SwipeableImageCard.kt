package nom4d.toysrevive.swipe

import android.os.Build

import android.view.HapticFeedbackConstants

import androidx.annotation.RequiresApi

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.Image
import androidx.compose.foundation.background

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding

import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape

import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Favorite
import androidx.compose.material.icons.filled.Info
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.Card
import androidx.compose.material3.CardDefaults
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.Text

import androidx.compose.runtime.Composable
import androidx.compose.runtime.rememberCoroutineScope

import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalView
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp

import com.alexstyl.swipeablecard.Direction
import com.alexstyl.swipeablecard.SwipeableCardState

import kotlinx.coroutines.launch

@RequiresApi(Build.VERSION_CODES.R)
@Composable
fun SwipeableImageCard(
    painter: Painter,
    contentDescription: String,
    imageTitle: String,
    state: SwipeableCardState,
    onDetailButtonClicked: () -> Unit,
    modifier: Modifier = Modifier
) {
    val scope = rememberCoroutineScope()
    val view = LocalView.current

    Card(
        modifier = modifier.fillMaxWidth(),
        shape = RoundedCornerShape(15.dp),
        elevation = CardDefaults.cardElevation(4.dp)
    ) {
        Box(modifier = Modifier.fillMaxHeight()) {
            Image(
                painter = painter,
                contentDescription = contentDescription,
                contentScale = ContentScale.Crop,
                modifier = Modifier.fillMaxSize()
            )
            Box(
                modifier = Modifier
                    .fillMaxSize()
                    .background(
                        brush = Brush.verticalGradient(
                            colors = listOf(Color.Transparent, Color.Black),
                            startY = 400f
                        )
                    )
            )
            Box(
                modifier = Modifier
                    .fillMaxSize()
                    .padding(10.dp), contentAlignment = Alignment.BottomStart
            ) {
                Column(verticalArrangement = Arrangement.SpaceBetween) {
                    Row(
                        horizontalArrangement = Arrangement.End,
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        Column(
                            verticalArrangement = Arrangement.SpaceBetween,
                            horizontalAlignment = Alignment.Start,
                            modifier = Modifier.fillMaxWidth(.9f)
                        ) {
                            Text(
                                text = imageTitle,
                                style = TextStyle(color = Color.White, fontSize = 25.sp)
                            )
                            Text(
                                text = contentDescription,
                                style = TextStyle(color = Color.White, fontSize = 16.sp)
                            )
                        }
                        IconButton(
                            onClick =  {
                                view.performHapticFeedback(HapticFeedbackConstants.CLOCK_TICK)
                                onDetailButtonClicked()
                            }
                        ) {
                            Icon(
                                Icons.Default.Info,
                                tint = Color.White,
                                contentDescription = "Click for more details"
                            )
                        }
                    }
                    Spacer(Modifier.height(25.dp))
                    Row(
                        horizontalArrangement = Arrangement.SpaceEvenly,
                        verticalAlignment = Alignment.CenterVertically,
                        modifier = Modifier.fillMaxWidth()
                    ) {
                        OutlinedButton(
                            onClick = {
                                scope.launch {
                                    state.swipe(Direction.Left)
//                                    onDislikeCard()
                                    view.performHapticFeedback(HapticFeedbackConstants.REJECT)
                                }
                            },
                            shape = CircleShape,
                            border = BorderStroke(1.dp, Color.Red),
                            colors = ButtonDefaults.outlinedButtonColors(
                                containerColor = Color.Transparent
                            )
                        ) {
                            Icon(
                                Icons.Default.Close,
                                tint = Color.Red,
                                contentDescription = "Dislike button"
                            )
                        }
                        OutlinedButton(
                            onClick = {
                                scope.launch {
                                    state.swipe(Direction.Right)
//                                    onLikedCard()
                                    view.performHapticFeedback(HapticFeedbackConstants.CONFIRM)
                                }
                            },
                            shape = CircleShape,
                            border = BorderStroke(1.dp, Color.Green),
                            colors = ButtonDefaults.outlinedButtonColors(
                                containerColor = Color.Transparent
                            )
                        ) {
                            Icon(
                                Icons.Default.Favorite,
                                tint = Color.Green,
                                contentDescription = "Like button"
                            )
                        }
                    }
                }
            }
        }
    }
}
