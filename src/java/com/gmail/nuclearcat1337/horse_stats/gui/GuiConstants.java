package com.gmail.nuclearcat1337.horse_stats.gui;

import net.minecraft.client.Minecraft;

/**
 * Created by Mr_Little_Kitty on 1/10/2017.
 */
public class GuiConstants
{
    public static final int STANDARD_TEXTBOX_HEIGHT = 20;
    public static final int STANDARD_BUTTON_HEIGHT = 20;

    public static final int STANDARD_SEPARATION_DISTANCE = 4;
    public static final int SMALL_SEPARATION_DISTANCE = 2;

    public static final int LONG_BUTTON_WIDTH = 200;
    public static final int MEDIUM_BUTTON_WIDTH = LONG_BUTTON_WIDTH/2;
    public static final int SMALL_BUTTON_WIDTH = MEDIUM_BUTTON_WIDTH/2;

    public static final int LARGE_TEXBOX_LENGTH = Minecraft.getMinecraft().fontRendererObj.getStringWidth("WWWWWWWWWWWWWWWWWWWWWWWW");
    public static final int MEDIUM_TEXBOX_LENGTH = LARGE_TEXBOX_LENGTH/2;
    public static final int SMALL_TEXBOX_LENGTH = LARGE_TEXBOX_LENGTH/3;

}
