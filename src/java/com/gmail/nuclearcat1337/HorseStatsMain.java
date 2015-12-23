package com.gmail.nuclearcat1337;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiChat;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.WorldRenderer;
import net.minecraft.client.renderer.entity.RenderManager;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
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

import java.text.DecimalFormat;
import java.util.ArrayList;

/*
Created by Mr_Little_Kitty on 12/17/2015
*/
@Mod(modid = HorseStatsMain.MODID, name = HorseStatsMain.MODNAME, version = HorseStatsMain.MODVERSION)
public class HorseStatsMain
{
    private static Minecraft mc = Minecraft.getMinecraft();

    public static final String MODID = "HorseStats";
    public static final String MODNAME = "Horse Stats";
    public static final String MODVERSION = "1.0.0";

    private static double perfectHorseSpeedThreshold = 13;	//max: 14.1?
    private static double goodHorseSpeedThreshold = 11;
    private static double badHorseSpeedThreshold = 9.5;		//min: ~7?

    private static double perfectHorseJumpThreshold = 5;	//max: 5.5?
    private static double goodHorseJumpThreshold = 4;
    private static double badHorseJumpThreshold = 2.5;		//min: 1.2

    private static int perfectHorseHPThreshold = 28;		//max: 30
    private static int goodHorseHPThreshold = 24;
    private static int badHorseHPThreshold = 20;			//min: 15

    /** Animals that are farther away than this will not have their info shown */
    private static int viewDistanceCutoff = 8;		//how far away we will render the overlay
    private static int maxViewDistanceCutoff = 120;

    public static boolean ShowTextBackgrounds = true;
    private static final int maxNumberOfOverlays = 200;	//render only the first nearest 50 overlays

    /** Sets the number of decimal places that will be rendered when displaying horse stats */
    public static int numberOfDecimalsDisplayed = 2;

    private static DecimalFormat decimalFormat = GetDecimalFormat();

    @Mod.EventHandler
    public void preInitialize(FMLPreInitializationEvent event)
    {

    }

    @Mod.EventHandler
    public void initialize(FMLInitializationEvent event)
    {
        MinecraftForge.EVENT_BUS.register(this);
    }

    /**
     * Gets the amount of decimals that should be displayed with a DecimalFormat object.
     * @return
     */
    private static DecimalFormat GetDecimalFormat()
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
    private static void RenderEntityInfoInWorld(Entity entity, float partialTickTime)
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
            if(i > maxNumberOfOverlays)
                return;

            EntityAgeable animal = (EntityAgeable)entity;

            if (animal.riddenByEntity instanceof EntityPlayer)
            {
                return;    //don't render stats of the horse/animal we are currently riding
            }

            //only show entities that are close by
            double distanceFromMe = mc.thePlayer.getDistanceSqToEntity(animal);

            if (distanceFromMe*distanceFromMe > maxViewDistanceCutoff
                    || distanceFromMe*distanceFromMe > viewDistanceCutoff)
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
     * Gets the max hearts an entity has
     * @param entity
     * @return e.x. Steve = 20 hit points
     */
    private static int GetEntityMaxHearts(EntityLivingBase entity)
    {
        return (int) Math.round(entity.getEntityAttribute(SharedMonsterAttributes.maxHealth).getAttributeValue() / 2);
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

    /**
     * Gets a horses speed, colored based on how good it is.
     * @param horse
     * @return e.x.:<br>aqua "13.5"<br>green "12.5"<br>white "11.3"<br>red "7.0"
     */
    private static String GetHorseSpeedText(EntityHorse horse)
    {
        double horseSpeed = GetEntityMaxSpeed(horse);
        String horseSpeedString = decimalFormat.format(horseSpeed);

        if (horseSpeed > perfectHorseSpeedThreshold)
            horseSpeedString = EnumChatFormatting.AQUA + horseSpeedString + EnumChatFormatting.WHITE;
        else if (horseSpeed > goodHorseSpeedThreshold)
            horseSpeedString = EnumChatFormatting.GREEN + horseSpeedString + EnumChatFormatting.WHITE;
        else if (horseSpeed < badHorseSpeedThreshold)
            horseSpeedString = EnumChatFormatting.RED + horseSpeedString + EnumChatFormatting.WHITE;

        return horseSpeedString;
    }

    /**
     * Gets a horses HP, colored based on how good it is.
     * @param horse
     * @return e.x.:<br>aqua "28"<br>green "26"<br>white "22"<br>red "18"
     */
    private static String GetHorseHPText(EntityHorse horse)
    {
        int horseHP = GetEntityMaxHP(horse);
        String horseHPString = decimalFormat.format(GetEntityMaxHP(horse));

        if (horseHP > perfectHorseHPThreshold)
            horseHPString = EnumChatFormatting.AQUA + horseHPString + EnumChatFormatting.WHITE;
        else if (horseHP > goodHorseHPThreshold)
            horseHPString = EnumChatFormatting.GREEN + horseHPString + EnumChatFormatting.WHITE;
        else if (horseHP < badHorseHPThreshold)
            horseHPString = EnumChatFormatting.RED + horseHPString + EnumChatFormatting.WHITE;

        return horseHPString;
    }

    /**
     * Gets a horses hearts, colored based on how good it is.
     * @param horse
     * @return e.x.:<br>aqua "15"<br>green "13"<br>white "11"<br>red "9"
     */
    private static String GetHorseHeartsText(EntityHorse horse)
    {
        int horseHP = GetEntityMaxHP(horse);
        int horseHearts = GetEntityMaxHearts(horse);
        String horseHeartsString = "" + horseHearts;

        if (horseHP > perfectHorseHPThreshold)
            horseHeartsString = EnumChatFormatting.AQUA + horseHeartsString + EnumChatFormatting.WHITE;
        else if (horseHP > goodHorseHPThreshold)
            horseHeartsString = EnumChatFormatting.GREEN + horseHeartsString + EnumChatFormatting.WHITE;
        else if (horseHP < badHorseHPThreshold)
            horseHeartsString = EnumChatFormatting.RED + horseHeartsString + EnumChatFormatting.WHITE;

        return horseHeartsString;
    }

    /**
     * Gets a horses jump height, colored based on how good it is.
     * @param horse
     * @return e.x.:<br>aqua "5.4"<br>green "4"<br>white "3"<br>red "1.5"
     */
    private static String GetHorseJumpText(EntityHorse horse)
    {
        double horseJump = GetHorseMaxJump(horse);
        String horseJumpString = decimalFormat.format(horseJump);

        if (horseJump > perfectHorseJumpThreshold)
            horseJumpString = EnumChatFormatting.AQUA + horseJumpString + EnumChatFormatting.WHITE;
        else if (horseJump > goodHorseJumpThreshold)
            horseJumpString = EnumChatFormatting.GREEN + horseJumpString + EnumChatFormatting.WHITE;
        else if (horseJump < badHorseJumpThreshold)
            horseJumpString = EnumChatFormatting.RED + horseJumpString + EnumChatFormatting.WHITE;

        return horseJumpString;
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
     * Gets a horses secondary coloring
     * @param horse
     * @return empty string if there is no secondary coloring (for donkeys)
     */
    private static String GetHorseMarkingText(EntityHorse horse)
    {
        String texture = horse.getVariantTexturePaths()[1];

        if(texture == null || texture.isEmpty())
            return "";

        String[] textureArray = texture.split("/");				//"textures/entity/horse/horse_markings_blackdots.png"
        texture = textureArray[textureArray.length-1];			//"horse_markings_blackdots.png"
        texture = texture.substring(15, texture.length()-4);	//"blackdots"
        texture = WordUtils.capitalize(texture);				//"Blackdots"

        return texture;
    }

    /**
     * Renders an overlay in the game world for the specified animal.
     * @param animal
     * @param partialTickTime
     */
    protected static void RenderAnimalOverlay(EntityAgeable animal, float partialTickTime)
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

            multilineOverlayArrayList.add(GetHorseSpeedText(horse) + " m/s");
            multilineOverlayArrayList.add(GetHorseHPText(horse) + " hp");
            multilineOverlayArrayList.add(GetHorseJumpText(horse) + " jump");

            if (animalGrowingAge < 0)
                multilineOverlayArrayList.add(GetHorseBabyGrowingAgeAsPercent(horse) + "%");
        }

        String[] multilineOverlayMessage = new String[1];
        multilineOverlayMessage = (String[])multilineOverlayArrayList.toArray(multilineOverlayMessage);

        if(multilineOverlayMessage[0] != null)
        {
            //render the overlay message
            RenderFloatingText(multilineOverlayMessage, x, y, z, 0xFFFFFF, ShowTextBackgrounds, partialTickTime);
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
