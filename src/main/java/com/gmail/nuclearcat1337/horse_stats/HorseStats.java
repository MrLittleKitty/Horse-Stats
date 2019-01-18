package com.gmail.nuclearcat1337.horse_stats;

import com.google.common.io.Files;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiChat;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.entity.RenderManager;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.passive.AbstractHorse;
import net.minecraft.entity.passive.EntityHorse;
import net.minecraftforge.client.event.RenderWorldLastEvent;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.event.FMLInitializationEvent;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import org.lwjgl.opengl.GL11;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.nio.Buffer;
import java.nio.charset.StandardCharsets;
import java.text.DecimalFormat;
import java.util.logging.Logger;

/*
Created by Mr_Little_Kitty on 12/17/2015
*/
@Mod(modid = HorseStats.MODID, name = HorseStats.MODNAME, version = HorseStats.MODVERSION)
public class HorseStats {
    public static final String MODID = "horsestats";
    public static final String MODNAME = "Horse Stats";
    public static final String MODVERSION = "1.3.0";

    public static final String DECIMAL_PLACES_KEY = "decimal-places";
    public static final String RENDER_KEY = "should-render";
    public static final String RENDER_DISTANCE_KEY = "render-distance";
    public static final String JUMP_KEY = "jump-threshold";
    public static final String SPEED_KEY = "speed-threshold";
    public static final String HEALTH_KEY = "health-threshold";

    private static Minecraft mc = Minecraft.getMinecraft();
    private static final File modSettingsFile = new File(mc.mcDataDir, "/mods/" + MODNAME + "/Settings.txt");

    @Mod.Instance(MODID)
    public static HorseStats instance;
    public static Logger logger = Logger.getLogger("HorseStats");

    private Settings settings;
    private DecimalFormat decimalFormat;

    private static final float MIN_TEXT_RENDER_SCALE = 0.02f;
    private static final float MAX_TEXT_RENDER_SCALE = 0.06f;

    private static final float scale_step = (MAX_TEXT_RENDER_SCALE - MIN_TEXT_RENDER_SCALE) / 30;

    @Mod.EventHandler
    public void preInitialize(FMLPreInitializationEvent event) {
        logger.info("HorseStats: pre-Initializing");

        initializeSettings();
    }

    @Mod.EventHandler
    public void initialize(FMLInitializationEvent event) {
        logger.info("HorseStats: Initializing");

        updateDecimalPlaces();

        //Self registers with forge to receive proper events
        new KeyHandler();

        MinecraftForge.EVENT_BUS.register(this);
    }

    public void updateDecimalPlaces() {
        decimalFormat = Util.CreateDecimalFormat(settings.decimalPlaces);
    }

    public int getDecimalPlaces() {
        return settings.decimalPlaces;
    }

    public Settings getSettings() {
        return settings;
    }

    public boolean shouldRenderStats() {
        return settings.shouldRender;
    }

    public float getRenderDistance() {
        return settings.renderDistance;
    }

    public float getRenderDistanceSquared() {
        return settings.renderDistance * settings.renderDistance;
    }

    public Threshold getSpeedThreshold() {
        return settings.speedThreshold;
    }

    public Threshold getJumpThreshold() {
        return settings.jumpThreshold;
    }

    public Threshold getHealthThreshold() {
        return settings.healthThreshold;
    }

    public void saveSettings() {
        if (!modSettingsFile.exists()) {
            try {
                modSettingsFile.mkdir();
                modSettingsFile.createNewFile();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        try {
            Files.write(gson.toJson(settings), modSettingsFile, StandardCharsets.UTF_8);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @SubscribeEvent
    public void RenderWorldLastEvent(RenderWorldLastEvent event) {
        if (mc.inGameHasFocus && shouldRenderStats()) {
            for (int i = 0; i < mc.world.loadedEntityList.size(); i++) {
                Object object = mc.world.loadedEntityList.get(i);

                if (object == null || !(object instanceof AbstractHorse)) {
                    continue;
                }

                RenderHorseInfoInWorld((AbstractHorse) object, event.getPartialTicks());
            }
        }
    }

    private void RenderHorseInfoInWorld(AbstractHorse horse, float partialTickTime) {
        //if the player is in the world
        //and not looking at a menu
        //and F3 not pressed
        //if ((mc.inGameHasFocus || mc.currentScreen == null || mc.currentScreen instanceof GuiChat) && !mc.gameSettings.showDebugInfo)
        if ((mc.inGameHasFocus || mc.currentScreen == null || mc.currentScreen instanceof GuiChat)) {
            if (mc.player.isRidingHorse() && mc.player.getRidingEntity() == horse)
                return;    //don't render stats of the horse/animal we are currently riding

            //only show entities that are close by
            double distanceFromMe = mc.player.getDistanceSq(horse);

            if (distanceFromMe > getRenderDistanceSquared())
                return;

            RenderHorseOverlay(horse, partialTickTime);
        }
    }

    protected void RenderHorseOverlay(AbstractHorse horse, float partialTickTime) {
        float x = (float) horse.posX;
        float y = (float) horse.posY;
        float z = (float) horse.posZ;

        //a positive value means the horse has bred recently
        int animalGrowingAge = horse.getGrowingAge();

        String[] overlayText = new String[animalGrowingAge < 0 ? 4 : 3];

        overlayText[0] = (getSpeedThreshold().format(decimalFormat, Util.GetEntityMaxSpeed(horse)) + " m/s");
        overlayText[1] = (getHealthThreshold().format(decimalFormat, Util.GetEntityMaxHP(horse)) + " hp");
        overlayText[2] = (getJumpThreshold().format(decimalFormat, Util.GetHorseMaxJump(horse)) + " jump");

        if (animalGrowingAge < 0)
            overlayText[3] = (Util.GetHorseBabyGrowingAgeAsPercent(horse) + "%");

        RenderFloatingText(overlayText, x, y + 1.3f, z, 0xFFFFFF, true, partialTickTime);
    }

    public void RenderFloatingText(String[] text, float x, float y, float z, int color, boolean renderBlackBackground, float partialTickTime) {
        //Thanks to Electric-Expansion mod for the majority of this code
        //https://github.com/Alex-hawks/Electric-Expansion/blob/master/src/electricexpansion/client/render/RenderFloatingText.java

        RenderManager renderManager = mc.getRenderManager();

        float playerX = (float) (mc.player.lastTickPosX + (mc.player.posX - mc.player.lastTickPosX) * partialTickTime);
        float playerY = (float) (mc.player.lastTickPosY + (mc.player.posY - mc.player.lastTickPosY) * partialTickTime);
        float playerZ = (float) (mc.player.lastTickPosZ + (mc.player.posZ - mc.player.lastTickPosZ) * partialTickTime);

        float dx = x - playerX;
        float dy = y - playerY;
        float dz = z - playerZ;
        float distance = (float) Math.sqrt(dx * dx + dy * dy + dz * dz);
        float scale = MIN_TEXT_RENDER_SCALE + (distance * scale_step);//.01f; //Min font scale for max text render distance

        GL11.glColor4f(1f, 1f, 1f, 0.5f);
        GL11.glPushMatrix();
        GL11.glTranslatef(dx, dy, dz);
        GL11.glRotatef(-renderManager.playerViewY, 0.0F, 1.0F, 0.0F);
        GL11.glRotatef(renderManager.playerViewX, 1.0F, 0.0F, 0.0F);
        GL11.glScalef(-scale, -scale, scale);
        GL11.glDisable(GL11.GL_LIGHTING);
        GL11.glDepthMask(false);
        GL11.glDisable(GL11.GL_DEPTH_TEST);
        GL11.glEnable(GL11.GL_BLEND);
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA);

        int textWidth = 0;
        for (String thisMessage : text) {
            int thisMessageWidth = mc.fontRenderer.getStringWidth(thisMessage);

            if (thisMessageWidth > textWidth)
                textWidth = thisMessageWidth;
        }

        int lineHeight = 10;
        int initialValue = lineHeight * text.length;

        if (renderBlackBackground) {
            int stringMiddle = textWidth / 2;

            Tessellator tessellator = Tessellator.getInstance();
            BufferBuilder vertexBuffer = tessellator.getBuffer();

            GlStateManager.disableTexture2D();

            //This code taken from 1.8.8 net.minecraft.client.renderer.entity.Render.renderLivingLabel()
            vertexBuffer.begin(7, DefaultVertexFormats.POSITION_COLOR);
            vertexBuffer.pos((double) (-stringMiddle - 1), (double) (-1) - initialValue, 0.0D).color(0.0F, 0.0F, 0.0F, 0.25F).endVertex();
            vertexBuffer.pos((double) (-stringMiddle - 1), (double) (8 + lineHeight * (text.length - 1)) - initialValue, 0.0D).color(0.0F, 0.0F, 0.0F, 0.25F).endVertex();
            vertexBuffer.pos((double) (stringMiddle + 1), (double) (8 + lineHeight * (text.length - 1)) - initialValue, 0.0D).color(0.0F, 0.0F, 0.0F, 0.25F).endVertex();
            vertexBuffer.pos((double) (stringMiddle + 1), (double) (-1) - initialValue, 0.0D).color(0.0F, 0.0F, 0.0F, 0.25F).endVertex();

            tessellator.draw();

            GlStateManager.enableTexture2D();
        }

        int i = 0;
        for (String message : text) {
            mc.fontRenderer.drawString(message, -textWidth / 2, (i * lineHeight) - initialValue, color);
            i++;
        }

        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
        GL11.glDepthMask(true);
        GL11.glEnable(GL11.GL_DEPTH_TEST);
        GL11.glPopMatrix();
    }

    private void initializeSettings() {
        if (modSettingsFile.exists()) {
            try (FileReader reader = new FileReader(modSettingsFile)) {
                Gson gson = new Gson();
                settings = gson.fromJson(reader, Settings.class);
                logger.info("Loaded settings from disk");
            } catch (IOException e) {
                settings = new Settings();
                e.printStackTrace();
                logger.info("Error loading settings from disk. Loading default settings");
            }
        } else {
            logger.info("No settings found. Loading default settings");
            settings = new Settings();
            saveSettings();
        }
    }
}
