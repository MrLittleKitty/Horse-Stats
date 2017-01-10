package com.gmail.nuclearcat1337.horse_stats;

import net.minecraft.client.Minecraft;
import net.minecraft.client.settings.KeyBinding;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.gameevent.InputEvent;
import org.lwjgl.input.Keyboard;

/**
 * Created by Mr_Little_Kitty on 1/10/2017.
 */
public class KeyHandler
{
    private final KeyBinding toggleButton = new KeyBinding("Horse Stats Menu", Keyboard.KEY_P,"Horse Stats");

    public KeyHandler()
    {
        FMLCommonHandler.instance().bus().register(this);
    }

    @SubscribeEvent
    public void onKeyPress(InputEvent.KeyInputEvent event)
    {
        if(toggleButton.isPressed())
        {
            //TODO---Show the horse stats menu screen
            Minecraft.getMinecraft().displayGuiScreen(null);
        }
    }
}
