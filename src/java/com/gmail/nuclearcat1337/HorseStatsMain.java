package com.gmail.nuclearcat1337;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiChat;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.WorldRenderer;
import net.minecraft.client.renderer.entity.RenderManager;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityAgeable;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.SharedMonsterAttributes;
import net.minecraft.entity.passive.EntityAnimal;
import net.minecraft.entity.passive.EntityHorse;
import net.minecraft.entity.passive.EntityVillager;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.EnumChatFormatting;
import net.minecraftforge.client.event.RenderWorldLastEvent;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.event.FMLInitializationEvent;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import org.apache.commons.lang3.text.WordUtils;
import org.lwjgl.opengl.GL11;

import java.io.File;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.logging.Logger;

/*
Created by Mr_Little_Kitty on 12/17/2015
*/
@Mod(modid = HorseStatsMain.MODID, name = HorseStatsMain.MODNAME, version = HorseStatsMain.MODVERSION)
public class HorseStatsMain
{
    public static final String MODID = "HorseStats";
    public static final String MODNAME = "Horse Stats";
    public static final String MODVERSION = "1.1.0";

    private static Minecraft mc = Minecraft.getMinecraft();
    public static Logger logger = Logger.getLogger("HorseStats");

    private ConfigData data;
    private DecimalFormat decimalFormat;

    @Mod.EventHandler
    public void preInitialize(FMLPreInitializationEvent event)
    {
        logger.info("HorseStats: pre-Initializing");
        File file = new File(Minecraft.getMinecraft().mcDataDir, "/mods/"+MODNAME);
        if(!file.exists())
            file.mkdir();

        data = new ConfigData(new File(file,"/config.txt").toString());
        decimalFormat = GetDecimalFormat(data.getNumberOfDecimals());
    }

    @Mod.EventHandler
    public void initialize(FMLInitializationEvent event)
    {
        logger.info("HorseStats: Initializing");
        MinecraftForge.EVENT_BUS.register(this);
    }

    /**
     * Gets the amount of decimals that should be displayed with a DecimalFormat object.
     * @return
     */
    private static DecimalFormat GetDecimalFormat(int numberOfDecimalsDisplayed)
    {
        if(numberOfDecimalsDisplayed < 1)
            return new DecimalFormat("#");

        String format = "#.";
        for(int i = 1; i <= numberOfDecimalsDisplayed; i++)
            format += "#";

        return new DecimalFormat(format);
    }

    /**
     * Event fired when the world gets rendered.
     * We render anything that need to be rendered into the game world in this method.
     * @param event
     */
    @SubscribeEvent
    public void RenderWorldLastEvent(RenderWorldLastEvent event)
    {
        if(mc.inGameHasFocus)
        {
            for (int i = 0; i < mc.theWorld.loadedEntityList.size(); i++)
            {
                Object object = mc.theWorld.loadedEntityList.get(i);

                if(object == null
                        || !(object instanceof EntityAnimal || object instanceof EntityVillager))
                {
                    continue;
                }

                RenderEntityInfoInWorld((Entity)object, event.partialTicks);
            }
        }
    }

    /**
     * Renders information about an entity into the game world.
     * @param entity
     * @param partialTickTime
     */
    private void RenderEntityInfoInWorld(Entity entity, float partialTickTime)
    {
        if (!(entity instanceof EntityAgeable))
        {
            return;    //we only care about ageable entities
        }

        int i = 0;

        //if the player is in the world
        //and not looking at a menu
        //and F3 not pressed
        if ((mc.inGameHasFocus || mc.currentScreen == null || mc.currentScreen instanceof GuiChat)
                && !mc.gameSettings.showDebugInfo)
        {
            if(i > data.getMaxNumberOfOverlays())
                return;

            EntityAgeable animal = (EntityAgeable)entity;

            if (animal.riddenByEntity instanceof EntityPlayer)
            {
                return;    //don't render stats of the horse/animal we are currently riding
            }

            //only show entities that are close by
            double distanceFromMe = mc.thePlayer.getDistanceSqToEntity(animal);

            if (distanceFromMe > data.getRenderDistanceSquared())
            {
                return;
            }

            RenderAnimalOverlay(animal, partialTickTime);
            i++;
        }
    }

    /**
     * Gets the baby horses age ranging from 0 to 100.
     * @param horse
     * @return
     */
    private static int GetHorseBabyGrowingAgeAsPercent(EntityHorse horse)
    {
        float horseGrowingAge = horse.getHorseSize();	//horse size ranges from 0.5 to 1
        return (int)((horseGrowingAge - 0.5f) * 2.0f * 100f);
    }

    /**
     * Gets the max height a horse can jump when the jump bar is fully charged.
     * @param horse
     * @return e.x. 1.2?-5.5?
     */
    private static double GetHorseMaxJump(EntityHorse horse)
    {
        //simulate gravity and air resistance to determine the jump height
        double yVelocity = horse.getHorseJumpStrength();	//horses's jump strength attribute
        double jumpHeight = 0;
        while (yVelocity > 0)
        {
            jumpHeight += yVelocity;
            yVelocity -= 0.08;
            yVelocity *= 0.98;
        }
        return jumpHeight;
    }

    /**
     * Gets an entity's max hit points
     * @param entity
     * @return e.x. Steve = 20 hit points
     */
    private static int GetEntityMaxHP(EntityLivingBase entity)
    {
        return (int) entity.getEntityAttribute(SharedMonsterAttributes.maxHealth).getAttributeValue();
    }

    /**
     * Gets an entity's max run speed in meters(blocks) per second
     * @param entity
     * @return e.x. Steve = 4.3 m/s. Horses ~7-13
     */
    private static double GetEntityMaxSpeed(EntityLivingBase entity)
    {
        //Steve has a movement speed of 0.1 and walks 4.3 blocks per second,
        //so multiply this result by 43 to convert to blocks per second
        return entity.getEntityAttribute(SharedMonsterAttributes.movementSpeed).getAttributeValue() * 43;
    }

    private String getFormattedText(final Threshold threshold, String initialString, double value)
    {
        String finalString = decimalFormat.format(value);

        if (value > threshold.getGood())
            finalString = EnumChatFormatting.AQUA + finalString + EnumChatFormatting.WHITE;
        else if (value > threshold.getAverage())
            finalString = EnumChatFormatting.GREEN + finalString + EnumChatFormatting.WHITE;
        else if (value < threshold.getBad())
            finalString = EnumChatFormatting.RED + finalString + EnumChatFormatting.WHITE;

        return finalString;
    }

    /**
     * Gets a horses primary coloring
     * @param horse
     * @return empty string if there is no coloring (for donkeys)
     */
    private static String GetHorseColoringText(EntityHorse horse)
    {
        String texture = horse.getVariantTexturePaths()[0];

        if(texture == null || texture.isEmpty())
            return "";

        String[] textureArray = texture.split("/");			//"textures/entity/horse/horse_creamy.png"
        texture = textureArray[textureArray.length-1];		//"horse_creamy.png"
        texture = texture.substring(6, texture.length()-4);	//"creamy"
        texture = WordUtils.capitalize(texture);			//"Creamy"

        return texture;
    }

    /**
     * Renders an overlay in the game world for the specified animal.
     * @param animal
     * @param partialTickTime
     */
    protected void RenderAnimalOverlay(EntityAgeable animal, float partialTickTime)
    {
        float x = (float)animal.posX;
        float y = (float)animal.posY;
        float z = (float)animal.posZ;

        //a positive value means the horse has bred recently
        int animalGrowingAge = animal.getGrowingAge();

        ArrayList multilineOverlayArrayList = new ArrayList(4);

        if(animal instanceof EntityHorse)
        {
            EntityHorse horse = (EntityHorse)animal;

            multilineOverlayArrayList.add(getFormattedText(data.getSpeedThreshold(),"",GetEntityMaxSpeed(horse)) + " m/s");
            multilineOverlayArrayList.add(getFormattedText(data.getHealthThreshold(), "", GetEntityMaxHP(horse)) + " hp");
            multilineOverlayArrayList.add(getFormattedText(data.getJumpThreshold(),"",GetHorseMaxJump(horse)) + " jump");

            if (animalGrowingAge < 0)
                multilineOverlayArrayList.add(GetHorseBabyGrowingAgeAsPercent(horse) + "%");
        }

        String[] multilineOverlayMessage = new String[1];
        multilineOverlayMessage = (String[])multilineOverlayArrayList.toArray(multilineOverlayMessage);

        if(multilineOverlayMessage[0] != null)
        {
            //render the overlay message
            RenderFloatingText(multilineOverlayMessage, x, y, z, 0xFFFFFF, data.showBackground(), partialTickTime);
        }
    }

    /**
     * Renders floating text in the 3D world at a specific position.
     * @param text The text to render
     * @param x X coordinate in the game world
     * @param y Y coordinate in the game world
     * @param z Z coordinate in the game world
     * @param color 0xRRGGBB text color
     * @param renderBlackBackground render a pretty black border behind the text?
     * @param partialTickTime Usually taken from RenderWorldLastEvent.partialTicks variable
     */
    public static void RenderFloatingText(String text, float x, float y, float z, int color, boolean renderBlackBackground, float partialTickTime)
    {
        String textArray[] = {text};
        RenderFloatingText(textArray, x, y, z, color, renderBlackBackground, partialTickTime);
    }

    /**
     * Renders floating lines of text in the 3D world at a specific position.
     * @param text The string array of text to render
     * @param x X coordinate in the game world
     * @param y Y coordinate in the game world
     * @param z Z coordinate in the game world
     * @param color 0xRRGGBB text color
     * @param renderBlackBackground render a pretty black border behind the text?
     * @param partialTickTime Usually taken from RenderWorldLastEvent.partialTicks variable
     */
    public static void RenderFloatingText(String[] text, float x, float y, float z, int color, boolean renderBlackBackground, float partialTickTime)
    {
        //Thanks to Electric-Expansion mod for the majority of this code
        //https://github.com/Alex-hawks/Electric-Expansion/blob/master/src/electricexpansion/client/render/RenderFloatingText.java

        RenderManager renderManager = mc.getRenderManager();

        float playerX = (float) (mc.thePlayer.lastTickPosX + (mc.thePlayer.posX - mc.thePlayer.lastTickPosX) * partialTickTime);
        float playerY = (float) (mc.thePlayer.lastTickPosY + (mc.thePlayer.posY - mc.thePlayer.lastTickPosY) * partialTickTime);
        float playerZ = (float) (mc.thePlayer.lastTickPosZ + (mc.thePlayer.posZ - mc.thePlayer.lastTickPosZ) * partialTickTime);

        float dx = x-playerX;
        float dy = y-playerY;
        float dz = z-playerZ;
        float distance = (float) Math.sqrt(dx*dx + dy*dy + dz*dz);
        float scale = 0.03f;

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
            int thisMessageWidth = mc.fontRendererObj.getStringWidth(thisMessage);

            if (thisMessageWidth > textWidth)
                textWidth = thisMessageWidth;
        }

        int lineHeight = 10;

        if(renderBlackBackground)
        {
            int stringMiddle = textWidth / 2;

            Tessellator tessellator = Tessellator.getInstance();
            WorldRenderer worldrenderer = tessellator.getWorldRenderer();

            GL11.glDisable(GL11.GL_TEXTURE_2D);
            //GlStateManager.disableTexture2D();

            ///* OLD 1.8 rendering code
            worldrenderer.startDrawingQuads();
            //worldrenderer.func_181668_a(7, DefaultVertexFormats.field_181709_i);	//field_181707_g maybe?

            GlStateManager.color(0.0F, 0.0F, 0.0F, 0.5F);
            worldrenderer.addVertex(-stringMiddle - 1, -1 + 0, 0.0D);
            worldrenderer.addVertex(-stringMiddle - 1, 8 + lineHeight*text.length-lineHeight, 0.0D);
            worldrenderer.addVertex(stringMiddle + 1, 8 + lineHeight*text.length - lineHeight, 0.0D);
            worldrenderer.addVertex(stringMiddle + 1, -1 + 0, 0.0D);

//            GlStateManager.color(0.0F, 0.0F, 0.0F, 0.5F);
//            worldrenderer.putPosition(-stringMiddle - 1, -1 + 0, 0.0D);
//            worldrenderer.putPosition(-stringMiddle - 1, 8 + lineHeight*text.length-lineHeight, 0.0D);
//            worldrenderer.putPosition(stringMiddle + 1, 8 + lineHeight*text.length-lineHeight, 0.0D);
//            worldrenderer.putPosition(stringMiddle + 1, -1 + 0, 0.0D);
           // */

            //This code taken from 1.8.8 net.minecraft.client.renderer.entity.Render.renderLivingLabel()
//            worldrenderer.func_181668_a(7, DefaultVertexFormats.field_181706_f);
//            worldrenderer.func_181662_b(-stringMiddle - 1, -1 + 0, 0.0D).func_181666_a(0.0F, 0.0F, 0.0F, 0.25F).func_181675_d();
//            worldrenderer.func_181662_b(-stringMiddle - 1, 8 + lineHeight*text.length-lineHeight, 0.0D).func_181666_a(0.0F, 0.0F, 0.0F, 0.25F).func_181675_d();
//            worldrenderer.func_181662_b(stringMiddle + 1, 8 + lineHeight*text.length-lineHeight, 0.0D).func_181666_a(0.0F, 0.0F, 0.0F, 0.25F).func_181675_d();
//            worldrenderer.func_181662_b(stringMiddle + 1, -1 + 0, 0.0D).func_181666_a(0.0F, 0.0F, 0.0F, 0.25F).func_181675_d();

            tessellator.draw();
            GL11.glEnable(GL11.GL_TEXTURE_2D);
            //GlStateManager.enableTexture2D();
        }

        int i = 0;
        for(String message : text)
        {
            mc.fontRendererObj.drawString(message, -textWidth / 2, i*lineHeight, color);
            i++;
        }

        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
        GL11.glDepthMask(true);
        GL11.glEnable(GL11.GL_DEPTH_TEST);
        GL11.glPopMatrix();
    }
}
