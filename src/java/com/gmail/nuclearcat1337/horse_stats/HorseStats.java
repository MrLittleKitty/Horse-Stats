package com.gmail.nuclearcat1337.horse_stats;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiChat;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.entity.RenderManager;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.passive.EntityHorse;
import net.minecraftforge.client.event.RenderWorldLastEvent;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.event.FMLInitializationEvent;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import org.lwjgl.opengl.GL11;

import java.io.File;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.entity.passive.AbstractHorse;
import net.minecraft.entity.passive.EntityZombieHorse;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.WordUtils;

/*
Created by Mr_Little_Kitty on 12/17/2015
 */
@Mod(modid = HorseStats.MODID, name = HorseStats.MODNAME, version = HorseStats.MODVERSION)
public class HorseStats
{

    public static final String MODID = "horsestats";
    public static final String MODNAME = "Horse Stats";
    public static final String MODVERSION = "2.0.0";

    public static final String DECIMAL_PLACES_KEY = "decimal-places";
    public static final String RENDER_KEY = "should-render";
    public static final String RENDER_DISTANCE_KEY = "render-distance";
    public static final String JUMP_KEY = "jump-threshold";
    public static final String SPEED_KEY = "speed-threshold";
    public static final String HEALTH_KEY = "health-threshold";

    public static final String SHOW_PATTERN_KEY = "show-pattern";
    public static final String SHOW_COLOR_KEY = "show-color";
    public static final String SHOW_HP_KEY = "show-hp";
    public static final String SHOW_JUMP_KEY = "show-jump";
    public static final String SHOW_SPEED_KEY = "show-speed";
    public static final String SHOW_HORSETYPE_KEY = "show-horsetype";
    public static final String SHOW_FOALSTATUS_KEY = "show-foalstatus";
    public static final String SHOW_TAMENESS_KEY = "show-tameness";
    public static final String SHOW_NAME_KEY = "show-name";

    private static Minecraft mc = Minecraft.getMinecraft();
    private static final String modSettingsFile = mc.mcDataDir + "/mods/" + MODNAME + "/Settings.txt";
    private static final long HORSE_WAIT_SECONDS = 5 * 60 * 1000;

    @Mod.Instance(MODID)
    public static HorseStats instance;
    public static Logger logger = Logger.getLogger("HorseStats");

    private Settings settings;
    private DecimalFormat decimalFormat;

    private static final float MIN_TEXT_RENDER_SCALE = 0.02f;
    private static final float MAX_TEXT_RENDER_SCALE = 0.06f;

    private float renderDistance;
    private static final float scale_step = (MAX_TEXT_RENDER_SCALE - MIN_TEXT_RENDER_SCALE) / 30;

    @Mod.EventHandler
    public void preInitialize(FMLPreInitializationEvent event)
    {
        logger.info("HorseStats: pre-Initializing");

        initializeSettings();

        updateRenderDistance();
    }

    @Mod.EventHandler
    public void initialize(FMLInitializationEvent event)
    {
        logger.info("HorseStats: Initializing");

        updateDecimalPlaces();

        //Self registers with forge to receive proper events
        new EventHandler();

        MinecraftForge.EVENT_BUS.register(this);
    }

    public void updateDecimalPlaces()
    {
        decimalFormat = Util.createDecimalFormat((Integer) settings.getValue(DECIMAL_PLACES_KEY));
    }

    public int getDecimalPlaces()
    {
        return (Integer) settings.getValue(DECIMAL_PLACES_KEY);
    }

    public void updateRenderDistance()
    {
        renderDistance = (Float) settings.getValue(RENDER_DISTANCE_KEY);
    }

    public Settings getSettings()
    {
        return settings;
    }

    public boolean shouldRenderStats()
    {
        return (Boolean) settings.getValue(RENDER_KEY);
    }

    public float getRenderDistance()
    {
        return renderDistance;
    }

    public float getRenderDistanceSquared()
    {
        return renderDistance * renderDistance;
    }

    public Threshold getSpeedThreshold()
    {
        return (Threshold) settings.getValue(SPEED_KEY);
    }

    public Threshold getJumpThreshold()
    {
        return (Threshold) settings.getValue(JUMP_KEY);
    }

    public Threshold getHealthThreshold()
    {
        return (Threshold) settings.getValue(HEALTH_KEY);
    }

    @SubscribeEvent
    public void RenderWorldLastEvent(RenderWorldLastEvent event)
    {
        if (mc.inGameHasFocus && shouldRenderStats())
        {
            for (int i = 0; i < mc.world.loadedEntityList.size(); i++)
            {
                Object object = mc.world.loadedEntityList.get(i);

                if (object == null || !(object instanceof AbstractHorse))
                {
                    continue;
                }

                RenderHorseInfoInWorld((AbstractHorse) object, event.getPartialTicks());
            }
        }
    }

    private void RenderHorseInfoInWorld(AbstractHorse horse, float partialTickTime)
    {
        //if the player is in the world
        //and not looking at a menu
        //and F3 not pressed
        //if ((mc.inGameHasFocus || mc.currentScreen == null || mc.currentScreen instanceof GuiChat) && !mc.gameSettings.showDebugInfo)
        if ((mc.inGameHasFocus || mc.currentScreen == null || mc.currentScreen instanceof GuiChat))
        {
            if (mc.player.isRidingHorse() && mc.player.getRidingEntity() == horse)
            {
                return;    //don't render stats of the horse/animal we are currently riding
            }
            //only show entities that are close by
            double distanceFromMe = mc.player.getDistanceSq(horse);

            if (renderDistance < 128 && distanceFromMe > getRenderDistanceSquared())
            {
                return;
            }

            RenderHorseOverlay(horse, partialTickTime);
        }
    }

    protected void RenderHorseOverlay(AbstractHorse horse, float partialTickTime)
    {
        float x = (float) horse.posX;
        float y = (float) horse.posY;
        float z = (float) horse.posZ;

        List<String> overlayText = new ArrayList<>(6);
        if ((Boolean) getSettings().getValue(SHOW_SPEED_KEY))
        {
            overlayText.add((getSpeedThreshold().format(decimalFormat, Util.getEntityMaxSpeed(horse)) + " m/s"));
        }
        if ((Boolean) getSettings().getValue(SHOW_HP_KEY))
        {
            overlayText.add((Util.getCurrentHealth(horse) + getHealthThreshold().format(decimalFormat, Util.getEntityMaxHP(horse)) + " hp"));
        }
        if ((Boolean) getSettings().getValue(SHOW_JUMP_KEY))
        {
            overlayText.add((getJumpThreshold().format(decimalFormat, Util.getHorseMaxJump(horse)) + " jump"));
        }
        if ((Boolean) getSettings().getValue(SHOW_PATTERN_KEY))
        {
            final String texture = horse instanceof EntityHorse ? Util.getHorseMarkingText((EntityHorse) horse) : "";
            if (texture != "")
            {
                overlayText.add(texture);
            }
        }

//        overlayText.add(Util.getHorseOwner(horse));
        String horseDetails = getHorseDetails(horse);
        if (!horseDetails.isEmpty())
        {
            overlayText.add(horseDetails);
        }
        final String name = horse.getCustomNameTag();
        if ((Boolean) getSettings().getValue(SHOW_NAME_KEY) && StringUtils.isNotEmpty(name))
        {
            overlayText.add(String.format("'%s'", name));
        }

        RenderFloatingText(overlayText.toArray(new String[]
        {
        }), x, y + 1.3f, z, 0xFFFFFF, true, partialTickTime);
    }

    public String getHorseDetails(AbstractHorse horse)
    {
        final List<String> details = new ArrayList<>();
        if ((Boolean) getSettings().getValue(SHOW_TAMENESS_KEY) && !horse.isChild())
        {
            details.add(!horse.isTame() ? "Wild" : "Tamed");
        }
        if ((Boolean) getSettings().getValue(SHOW_FOALSTATUS_KEY))
        {
            details.add(horse.isChild() ? "Foal" : "Adult");
        }
        if ((Boolean) getSettings().getValue(SHOW_HORSETYPE_KEY))
        {
            details.add(Util.getHorseTypeName(horse.getClass()));
        }
        return String.join(" ", details.toArray(new String[]
        {
        }));
    }

    public void RenderFloatingText(String[] text, float x, float y, float z, int color, boolean renderBlackBackground, float partialTickTime)
    {
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
        for (String thisMessage : text)
        {
            int thisMessageWidth = mc.fontRenderer.getStringWidth(thisMessage);

            if (thisMessageWidth > textWidth)
            {
                textWidth = thisMessageWidth;
            }
        }

        int lineHeight = 10;
        int initialValue = lineHeight * text.length;

        if (renderBlackBackground)
        {
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
        for (String message : text)
        {
            mc.fontRenderer.drawString(message, -textWidth / 2, (i * lineHeight) - initialValue, color);
            i++;
        }

        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
        GL11.glDepthMask(true);
        GL11.glEnable(GL11.GL_DEPTH_TEST);
        GL11.glPopMatrix();
    }

    private void initializeSettings()
    {
        File file = new File(Minecraft.getMinecraft().mcDataDir, "/mods/" + MODNAME);
        if (!file.exists())
        {
            file.mkdir();
        }

        settings = new Settings(new File(modSettingsFile), parser);
        settings.loadSettings();

        //Threshold(bad,average,good)
        settings.setValueIfNotSet(JUMP_KEY, new Threshold(4, 5));
        settings.setValueIfNotSet(SPEED_KEY, new Threshold(11, 13));
        settings.setValueIfNotSet(HEALTH_KEY, new Threshold(24, 28));

        settings.setValueIfNotSet(RENDER_DISTANCE_KEY, 15.0F);
        settings.setValueIfNotSet(RENDER_KEY, Boolean.TRUE);
        settings.setValueIfNotSet(DECIMAL_PLACES_KEY, 3);

        settings.setValueIfNotSet(SHOW_COLOR_KEY, Boolean.TRUE);
        settings.setValueIfNotSet(SHOW_FOALSTATUS_KEY, Boolean.TRUE);
        settings.setValueIfNotSet(SHOW_HORSETYPE_KEY, Boolean.TRUE);
        settings.setValueIfNotSet(SHOW_HP_KEY, Boolean.TRUE);
        settings.setValueIfNotSet(SHOW_JUMP_KEY, Boolean.TRUE);
        settings.setValueIfNotSet(SHOW_NAME_KEY, Boolean.TRUE);
        settings.setValueIfNotSet(SHOW_PATTERN_KEY, Boolean.TRUE);
        settings.setValueIfNotSet(SHOW_SPEED_KEY, Boolean.TRUE);
        settings.setValueIfNotSet(SHOW_TAMENESS_KEY, Boolean.TRUE);

        settings.saveSettings();
    }

    private static final Settings.ValueParser parser = new Settings.ValueParser()
    {
        @Override
        public Object parse(String key, String value)
        {
            if (key.contains("threshold"))
            {
                return Threshold.fromString(value);
            } else if (value.equalsIgnoreCase(Boolean.FALSE.toString()))
            {
                return Boolean.FALSE;
            } else if (value.equalsIgnoreCase(Boolean.TRUE.toString()))
            {
                return Boolean.TRUE;
            } else if (key.equalsIgnoreCase(RENDER_DISTANCE_KEY))
            {
                return Float.parseFloat(value);
            } else
            {
                try
                {
                    return Integer.parseInt(value);
                } catch (NumberFormatException e)
                {
                    //Return a string
                    return value;
                }
            }
        }
    };
}
