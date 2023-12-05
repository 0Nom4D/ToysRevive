package nom4d.toysrevive.swipe

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Favorite
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.Icon
import androidx.compose.material3.OutlinedButton
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp

@Composable
fun InteractionButtons(onLikeButtonClicked: () -> Unit, onDislikeButtonClicked: () -> Unit) {
    Row(
        horizontalArrangement = Arrangement.SpaceEvenly,
        verticalAlignment = Alignment.CenterVertically,
        modifier = Modifier.fillMaxWidth()
    ) {
        OutlinedButton(
            onClick = { onLikeButtonClicked() },
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
            onClick = { onDislikeButtonClicked() },
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
