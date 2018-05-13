package com.gmail.nuclearcat1337.horse_stats;

import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.SharedMonsterAttributes;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.client.Minecraft;
import net.minecraft.entity.passive.AbstractHorse;
import net.minecraft.entity.passive.EntityDonkey;
import net.minecraft.entity.passive.EntityHorse;
import net.minecraft.entity.passive.EntityMule;
import net.minecraft.entity.player.EntityPlayer;
import org.apache.commons.lang3.text.WordUtils;

/**
 * Created by Mr_Little_Kitty on 1/10/2017.
 */
public class Util
{

    /**
     * Gets the amount of decimals that should be displayed with a DecimalFormat
     * object.
     *
     * @return
     */
    public static DecimalFormat createDecimalFormat(int numberOfDecimalsDisplayed)
    {
        if (numberOfDecimalsDisplayed < 1)
        {
            return new DecimalFormat("#");
        }

        String format = "#.";
        for (int i = 1; i <= numberOfDecimalsDisplayed; i++)
        {
            format += "#";
        }

        return new DecimalFormat(format);
    }

    /**
     * Gets the max height a horse can jump when the jump bar is fully charged.
     *
     * @param horse
     * @return e.x. 1.2?-5.5?
     */
    public static float getHorseMaxJump(AbstractHorse horse)
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
        return (float) jumpHeight;
    }

    /**
     * Gets the owner of the horse.
     *
     * @param horse
     * @return doesn't seem to work...
     */
    public static String getHorseOwner(AbstractHorse horse)
    {
        final String owner;
        UUID ownerid = horse.getOwnerUniqueId();
        if (ownerid != null) {
            EntityPlayer player = Minecraft.getMinecraft().world.getPlayerEntityByUUID(ownerid);
            if (player != null) {
                owner = player.getName();
            } else {
                owner = "belongs to null player";
            }
        } else {
                owner = "no owner";
        }
        final String team;
        if (horse.getTeam() != null) {
            team = horse.getTeam().getName();
        } else {
            team = "no team";
        }
        return owner + ":" + team;
    }

    /**
     * Gets an entity's max run speed in meters(blocks) per second
     *
     * @param entity
     * @return e.x. Steve = 4.3 m/s. Horses ~7-13
     */
    public static float getEntityMaxSpeed(EntityLivingBase entity)
    {
        //Steve has a movement speed of 0.1 and walks 4.3 blocks per second,
        //so multiply this result by 43 to convert to blocks per second
        return (float) entity.getEntityAttribute(SharedMonsterAttributes.MOVEMENT_SPEED).getAttributeValue() * 43;
    }

    /**
     * Gets an entity's max hit points
     *
     * @param entity
     * @return e.x. Steve = 20 hit points
     */
    public static int getEntityMaxHP(EntityLivingBase entity)
    {
        return (int) entity.getEntityAttribute(SharedMonsterAttributes.MAX_HEALTH).getAttributeValue();
    }

    /**
     * Gets the baby horses age ranging from 0 to 100.
     *
     * @param horse
     * @return
     */
    public static int getHorseBabyGrowingAgeAsPercent(AbstractHorse horse)
    {
        float horseGrowingAge = horse.getHorseSize();	//horse size ranges from 0.5 to 1
        return (int) ((horseGrowingAge - 0.5f) * 2.0f * 100f);
    }

    /**
     * Gets a horses coloring
     *
     * @param horse
     * @return empty string if there is no coloring
     */
    public static String getHorseMarkingText(EntityHorse horse)
    {
        return Arrays.stream(horse.getVariantTexturePaths())
                .filter(s -> s != null)
                .map(s -> s.split("[\\p{Punct}\\s]+"))
                .filter(a -> a.length > 2)
                .map(a -> a[a.length - 2])
                .reduce((a, b) -> String.join(" ", b, a))
                .map(WordUtils::capitalizeFully)
                .orElse("");
    }

    public static String getHorseTypeName(Class c)
    {
        final String start = c.getSimpleName().replace("Entity", "").replace("Horse", "");
        return start.length() == 0 ? "Horse" : start;
    }

    public static String getCurrentHealth(AbstractHorse horse)
    {
        return String.format("%d/", (int) horse.getHealth());
    }

    public static String getHorseTameness(AbstractHorse horse)
    {
        return horse.isChild() ? (!horse.isTame() ? "Wild" : "Tamed") : "";
    }

    public static String getHorseFoalStatus(AbstractHorse horse)
    {
        return horse.isChild() ? "Foal" : "Adult";
    }

}
