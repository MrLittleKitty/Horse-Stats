package com.gmail.nuclearcat1337.horse_stats;

import com.gmail.nuclearcat1337.horse_stats.gui.GuiHorseStats;
import java.util.HashMap;
import java.util.Map;
import net.minecraft.client.Minecraft;
import net.minecraft.client.settings.KeyBinding;
import net.minecraft.entity.passive.EntityHorse;
import net.minecraftforge.event.entity.living.BabyEntitySpawnEvent;
import net.minecraftforge.fml.client.registry.ClientRegistry;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.gameevent.InputEvent;
import org.lwjgl.input.Keyboard;

/**
 * Created by Mr_Little_Kitty on 1/10/2017.
 */
public class EventHandler {

    private final KeyBinding toggleButton = new KeyBinding("Horse Stats Menu", Keyboard.KEY_P, "Horse Stats");

    public EventHandler() {
        ClientRegistry.registerKeyBinding(toggleButton);
        FMLCommonHandler.instance().bus().register(this);
    }

    @SubscribeEvent
    public void onBabySpawn(BabyEntitySpawnEvent event) {
        try {
            if (event instanceof BabyEntitySpawnEvent) {
                BabyEntitySpawnEvent spawn = (BabyEntitySpawnEvent) event;
                if (spawn.getChild() instanceof EntityHorse) {
                    long time = System.currentTimeMillis();
                    HorseStats.SPAWN_PARENT_TIMES.put(spawn.getParentA().getEntityId(), time);
                    HorseStats.SPAWN_PARENT_TIMES.put(spawn.getParentB().getEntityId(), time);
                    HorseStats.CHILD_TIMES.put(spawn.getChild().getEntityId(), time);
                }

            }
        } catch (Exception e) {
        }
    }

    @SubscribeEvent
    public void onKeyPress(InputEvent.KeyInputEvent event) {
        if (toggleButton.isPressed()) {
            Minecraft.getMinecraft().displayGuiScreen(new GuiHorseStats(HorseStats.instance));
        }
    }
}
