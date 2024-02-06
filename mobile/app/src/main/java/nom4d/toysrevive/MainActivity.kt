package nom4d.toysrevive

import android.os.Build
import android.os.Bundle

import androidx.activity.compose.setContent
import androidx.annotation.RequiresApi
import androidx.appcompat.app.AppCompatActivity
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.Scaffold
import androidx.compose.ui.Modifier

import dagger.hilt.android.AndroidEntryPoint

import nom4d.toysrevive.auth.AuthNavigationHost
import nom4d.toysrevive.ui.theme.ToysReviveTheme

@AndroidEntryPoint
class MainActivity : AppCompatActivity() {
    @RequiresApi(Build.VERSION_CODES.R)
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        setContent {
            ToysReviveTheme {
                Scaffold(modifier = Modifier.fillMaxSize()) {
                    AuthNavigationHost(modifier = Modifier.padding(it))
                }
            }
        }
    }
}
